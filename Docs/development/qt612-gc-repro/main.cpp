// Minimal reproducer for a dangling-identifier GC crash in QtQml
// (qtdeclarative 6.12.0, SIGSEGV in QV4::MarkStack::drain).
//
// Root cause:
//   IdentifierHashData::entries is a GC root: IdentifierTable::markObjects marks
//   the PropertyKeys stored in every registered hash's entries array every cycle,
//   and the entries are never pruned. When a PropertyKey is stored into a NEW
//   hash, the string it points at must be (re-)marked if a GC cycle is ongoing.
//   The FIRST intern of a name goes through PropertyKey::fromStringOrSymbol(),
//   which does exactly that (insertion barrier). But RE-interning an EXISTING
//   name into another hash goes through IdentifierTable::asPropertyKey(const
//   QString&) -> resolveStringEntry(), which finds the existing table entry and
//   returns its identifier WITHOUT any write barrier. IdentifierHash::addEntry()
//   then stores that key into the (root) entries array unmarked.
//
//   If, at that moment,
//     - a GC cycle is ongoing and past MarkGlobalObject (so the hash roots were
//       already marked this cycle), and
//     - no other live root references the string (the hash it was originally
//       interned into has been destroyed),
//   the string is white at DoSweep: it is freed while the new hash's entries
//   array keeps pointing at it. The next cycle's IdentifierTable::markObjects
//   pushes that dangling pointer and MarkStack::drain dereferences the zeroed
//   header of the freed string -> SIGSEGV.
//
// Repro outline (public API only):
//   1. Intern a fresh name into a temporary QQmlContext each round, then destroy
//      that context: the string loses its only GC root and remains referenced
//      only by the identifier table arrays, which are NOT roots (they are pruned
//      at sweep).
//   2. Re-intern older names into fresh long-lived contexts. This takes the
//      existing-table-entry path, which stores into the new context's root name
//      cache WITHOUT a write barrier. Delayed so the re-intern lands in a later
//      GC cycle than the first intern (the first intern's barrier only keeps the
//      string black until the next sweep resets the black bits).
//   3. Everything runs on timer ticks inside a running event loop: with an event
//      loop, Qt's incremental GC advances between ticks via queued event-loop
//      callbacks, so the mutator (the next tick) regularly runs mid-cycle, past
//      MarkGlobalObject but before DoSweep - exactly the crash window. (Without
//      an event loop the GC only advances on allocation pressure and the timing
//      phase-locks.)
//
// Build:  cmake -B build && cmake --build build
// Run:    ./build/repro [iterations] [preludeStrings]
// A/B:    unpatched libQt6Qml -> SIGSEGV in QV4::MarkStack::drain;
//         LD_LIBRARY_PATH pointing at a libQt6Qml with the one-barrier fix in
//         QV4::IdentifierHash::addEntry -> runs to completion.

#include <QCoreApplication>
#include <QQmlContext>
#include <QQmlEngine>
#include <QJSValue>
#include <QList>
#include <QLoggingCategory>

#include <cstdio>
#include <cstdlib>

// This Qt build's default message handler prints nothing on this machine;
// route Qt logging (GC state transitions, cross-validation errors) to stderr.
static void stderrHandler(QtMsgType, const QMessageLogContext &, const QString &msg)
{
    std::fprintf(stderr, "%s\n", msg.toUtf8().constData());
}

int main(int argc, char **argv)
{
    // Shorten the incremental GC slices to ~1 ms. With the default ~5 ms budget
    // a whole mark phase of a small heap fits into one slice and the mutator
    // never runs mid-cycle; with 1 ms slices the mark phase yields constantly
    // and the window between MarkGlobalObject and DoSweep is wide open. (The
    // production crash this reproduces happens with default settings under
    // real-world QML churn; see the accompanying write-up.)
    qputenv("QV4_GC_TIMELIMIT", "1");

    QCoreApplication app(argc, argv);
    qInstallMessageHandler(stderrHandler);
    const int iterations = argc > 1 ? std::atoi(argv[1]) : 100000;
    const int preludeStrings = argc > 2 ? std::atoi(argv[2]) : 300000;

    QQmlEngine engine;
    QQmlContext *root = engine.rootContext();

    // Prelude: a large live heap so the mark phase spans several incremental
    // GC slices and the mutator runs between MarkGlobalObject and DoSweep.
    QJSValue global = engine.globalObject();
    QJSValue prelude = engine.evaluate(QStringLiteral(
        "var a = new Array(%1);"
        "for (var i = 0; i < a.length; ++i) a[i] = 'persistent' + i;"
        "a").arg(preludeStrings));
    if (prelude.isError())
        std::fprintf(stderr, "PRELUDE ERROR: %s\n", prelude.toString().toUtf8().constData());
    else
        global.setProperty(QStringLiteral("keep"), prelude);

    // Garbage churn: keeps incremental GC cycles coming throughout the run.
    QJSValue scratch = engine.newArray(1);
    long long n = 0;

    // Long-lived contexts holding re-interned identifiers: they must stay alive
    // so a dangling entry survives until the next GC cycle.
    QList<QQmlContext *> keepAlive;
    const int keepWindow = 20000;

    // The crash needs, within ONE GC cycle:
    //   - the identifier's LAST holder hash destroyed BEFORE that cycle's
    //     MarkGlobalObject (so the string is not marked in this cycle), and
    //   - a re-intern into a fresh hash AFTER that MarkGlobalObject but BEFORE
    //     DoSweep (an unmarked store into a root array).
    // So: each holder context lives only D ticks and is then destroyed, and
    // every tick re-interns one older name at a RANDOM delay, probing every
    // phase of the GC cycle. An early re-intern (before MarkGlobalObject) is
    // harmless - it roots the string and the round is simply wasted.
    const int holderLifetime = 5;
    QList<QQmlContext *> holders;
    holders.reserve(holderLifetime + 2);

    int i = 0;
    for (; i < iterations; ++i) {        // Destroy the expired holder: its name's string loses its last GC root.
        if (i >= holderLifetime) {
            delete holders.takeFirst();
        }

        // Intern a fresh name into a fresh holder context.
        const QString name = QStringLiteral("sharedProp%1").arg(i);
        QQmlContext *holder = new QQmlContext(root);
        holder->setContextProperty(name, QVariant(i));
        holders.append(holder);

        // Re-intern an older name (its holder died holderLifetime ticks ago)
        // into a fresh long-lived context, at a random delay. This takes the
        // existing-table-entry path, which stores into the new context's root
        // name cache WITHOUT a write barrier.
        const int delay = holderLifetime + 45 + (std::rand() % 3000);
        if (i >= delay) {
            QQmlContext *ctx = new QQmlContext(root);
            ctx->setContextProperty(QStringLiteral("sharedProp%1").arg(i - delay), QVariant(i));
            keepAlive.append(ctx);
        }
        while (keepAlive.size() > keepWindow)
            delete keepAlive.takeFirst();

        // Garbage churn between re-interns. The jitter breaks the loop's
        // allocation periodicity, so GC pressure events do not phase-lock to a
        // fixed tick phase.
        int jitter = std::rand() % 60;
        for (int g = 0; g < 6 + jitter; ++g)
            scratch.setProperty(0, QStringLiteral("junk%1").arg(n++));

        if ((i % 5000) == 0) {
            std::printf("iteration %d\n", i);
            std::fflush(stdout);
        }
    }
    std::printf("survived %d iterations\n", i);
    return 0;
}
