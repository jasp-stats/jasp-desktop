// Minimal reproducer for two missing-barrier defects in QtQml's incremental
// garbage collector (qtdeclarative 6.12.0), SIGSEGV / missed objects.
//
// Scenario "intern" (default) — dangling identifier-hash root:
//   IdentifierHashData::entries is a GC root: IdentifierTable::markObjects()
//   marks the PropertyKeys stored in every registered hash's entries array at
//   the start of every cycle, and the entries are never pruned. The FIRST
//   intern of a name is safe (PropertyKey::fromStringOrSymbol carries an
//   insertion barrier). But re-interning an EXISTING name into another hash
//   goes through IdentifierTable::asPropertyKey(const QString&) ->
//   resolveStringEntry(), which finds the existing table entry and returns its
//   identifier WITHOUT any barrier; IdentifierHash::addEntry() then stores it
//   into the new hash's root array unmarked. When the previous holder hash was
//   destroyed earlier in the same cycle and the re-intern lands after
//   MarkGlobalObject but before DoSweep, the sweep frees the string while the
//   root array keeps the dangling pointer; the next cycle's marking crashes:
//   SIGSEGV in QV4::MarkStack::drain / Heap::Base::mark.
//
//   The trigger: QQmlContext::setContextProperty() with a name whose previous
//   holder context was destroyed. Each round interns a fresh name into a
//   short-lived holder context, then destroys it; an older name is re-interned
//   into a fresh long-lived context at a randomized delay.
//
// Scenario "weak" — QObjectWrapper missed by the weak-values pass:
//   Early in each cycle the collector walks all QObjectWrappers once
//   (MarkWeakValues) and marks those whose QObject — or the root of its parent
//   chain — keeps them alive (QQmlData::keepAliveDuringGarbageCollection: no
//   QQmlData, indestructible, or rootObjectInCreation). A QObject that becomes
//   rooted (e.g. setParent() into a keep-alive chain) AFTER the pass ran in the
//   current cycle keeps a wrapper the collector never marks; the sweep frees
//   the wrapper (and its property/method storage) while the QObject is still
//   alive and in use. Under real QML form churn this is the crash JASP
//   works around with installQmlGcRescan (Desktop/utilities/qmlgcrescan.cpp).
//
//   The trigger: component-created QML objects (their own keepAlive() is false
//   once creation completes) get a wrapper via a QVariant-based JS store
//   (newQObject() would set CppOwnership->indestructible and make the wrapper
//   unmissable), the JS reference is dropped so the wrapper is only weakly
//   held, and a random-delayed setParent() under a plain C++ QObject (which has
//   no QQmlData, hence is a permanent keep-alive anchor) lands after the pass.
//
// Both scenarios share a prelude (a large live heap so a mark phase cannot
// finish within one GC slice) and QV4_GC_TIMELIMIT=1 (1 ms slices, so the
// mutator regularly runs mid-cycle; a toy-size heap otherwise completes whole
// cycles atomically). The production crashes happen with default settings
// because real QML churn dwarfs these toy loops.
//
// Build:  cmake -B build && cmake --build build
// Run:    ./build/repro intern|weak [iterations] [preludeStrings]
//
// A/B (Qt 6.12.0, x86_64 Linux):
//   intern + unpatched libQt6Qml  -> SIGSEGV 3/3 runs in ~1 s
//   intern + one-barrier fix      -> clean 4/4 runs
//   weak   + crossvalidator       -> "Cross Validation Error ... Object" per
//                                    cycle on the unpatched lib, none with the
//                                    weak-pass-rerun fix (the same metric JASP
//                                    used to validate its installQmlGcRescan).

#include <QCoreApplication>
#include <QPointer>
#include <QQmlComponent>
#include <QQmlContext>
#include <QQmlError>
#include <QQmlEngine>
#include <QJSValue>
#include <QList>
#include <QLoggingCategory>
#include <QUrl>
#include <QVariant>
#include <cstdio>
#include <cstdlib>

// This Qt build's default message handler prints nothing on this machine;
// route Qt logging (GC state transitions, cross-validation errors) to stderr.
static void stderrHandler(QtMsgType, const QMessageLogContext &, const QString &msg)
{
    std::fprintf(stderr, "%s\n", msg.toUtf8().constData());
}

static void growPrelude(QQmlEngine &engine, int preludeStrings)
{
    // A large live heap so the mark phase spans several incremental GC slices
    // and the mutator runs between MarkGlobalObject and DoSweep.
    QJSValue prelude = engine.evaluate(QStringLiteral(
        "var a = new Array(%1);"
        "for (var i = 0; i < a.length; ++i) a[i] = 'persistent' + i;"
        "a").arg(preludeStrings));
    if (prelude.isError())
        std::fprintf(stderr, "PRELUDE ERROR: %s\n", prelude.toString().toUtf8().constData());
    else
        engine.globalObject().setProperty(QStringLiteral("keep"), prelude);
}

// Bug: dangling identifier-hash root after re-interning an existing name into
// a new hash while its previous holder hash is dead (see header).
static void runInternScenario(QQmlEngine &engine, int iterations)
{
    QQmlContext *root = engine.rootContext();

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

    for (int i = 0; i < iterations; ++i) {
        // Destroy the expired holder: its name's string loses its last GC root.
        if (i >= holderLifetime)
            delete holders.takeFirst();

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

    qDeleteAll(keepAlive);
    qDeleteAll(holders);
    std::printf("survived %d iterations\n", iterations);
}

// Bug: the weak-values pass decides keep-alive once per cycle; a QObject rooted
// after the pass keeps a wrapper the collector never marks (see header).
static void runWeakScenario(QQmlEngine &engine, int iterations)
{
    // A plain C++ QObject has no QQmlData, so
    // QQmlData::keepAliveDuringGarbageCollection(anchor) == true forever:
    // anything parented under it is kept alive through the weak pass's
    // parent-chain check.
    QObject anchor;

    // A component-created QML object has QQmlData; with JavaScriptOwnership its
    // indestructible flag is cleared (qjsengine.cpp: ownership != CppOwnership
    // => indestructible = false), so the object's own keepAlive() is false
    // while it is unparented and its wrapper is missable.
    const QString victimQml = QStringLiteral("import QtQml\nQtObject { property int p: 1 }\n");
    QQmlComponent victimComponent(&engine);
    // A file-scheme URL: unknown schemes (e.g. a custom one) load asynchronously
    // and create() would fail with "Component is not ready". The file itself
    // does not need to exist; setData() supplies the source.
    victimComponent.setData(victimQml.toUtf8(), QUrl::fromLocalFile(QStringLiteral("repro-victim.qml")));
    if (victimComponent.isError()) {
        const QList<QQmlError> errs = victimComponent.errors();
        for (const QQmlError &e : errs)
            std::fprintf(stderr, "COMPONENT ERROR: %s\n", e.toString().toUtf8().constData());
    }

    QList<QPointer<QObject>> pending;   // victims awaiting parenting (QPointer: the
                                        // collector may legitimately delete unparented,
                                        // JS-owned, unreferenced victims)
    QList<QPointer<QObject>> rooted;    // parented under the anchor (wrapper SHOULD be kept)
    int parentedCount = 0;
    int createFailCount = 0;
    int deadCount = 0;

    QJSValue scratch = engine.newArray(1);
    long long n = 0;

    for (int i = 0; i < iterations; ++i) {
        // 1. A fresh victim whose wrapper is only weakly held: create the object,
        //    make the engine wrap it via a QVariant-based JS store (newQObject()
        //    would set CppOwnership -> indestructible and make the wrapper
        //    unmissable), then drop the JS reference again.
        QObject *victim = victimComponent.create();
        if (!victim)
            ++createFailCount;
        engine.setObjectOwnership(victim, QQmlEngine::JavaScriptOwnership);
        const QString propName = QStringLiteral("victim%1").arg(i);
        engine.globalObject().setProperty(propName, engine.toScriptValue(QVariant::fromValue(victim)));
        engine.globalObject().deleteProperty(propName);
        pending.append(victim);

        // Root an older victim under the keep-alive anchor after a SHORT random
        // delay: the wrapper dies at the first sweep after its creation, so the
        // parenting must land within a cycle. If it lands after the weak pass
        // already ran in the current cycle, the pass never marks the wrapper
        // even though the object is now rooted; the sweep frees the wrapper
        // while the QObject is alive and rooted.
        if (pending.size() > 2 + (std::rand() % 25)) {
            QPointer<QObject> v = pending.takeFirst();
            if (v) {                    // skip victims the collector already deleted
                v->setParent(&anchor);
                ++parentedCount;
                rooted.append(v);
            } else {
                ++deadCount;
            }
        }

        // Keep the live set small so the incremental GC keeps running: victims
        // not parented in time and victims past their usefulness are deleted
        // from C++ (their wrappers then become ordinary garbage). The rooted
        // victims must outlive ONE full GC cycle (~5000 rounds here): a wrapper
        // that is missed by the weak pass is only freed at that cycle's sweep,
        // so deleting them earlier hides the bug entirely.
        while (pending.size() > 40) {
            QPointer<QObject> old = pending.takeFirst();
            if (old)
                delete old.data();
        }
        while (rooted.size() > 8000) {
            QPointer<QObject> old = rooted.takeFirst();
            if (old)
                delete old.data();
        }

        // Garbage churn, with jitter (same rationale as the intern scenario).
        int jitter = std::rand() % 60;
        for (int g = 0; g < 6 + jitter; ++g)
            scratch.setProperty(0, QStringLiteral("junk%1").arg(n++));

        if ((i % 5000) == 0) {
            std::printf("iteration %d\n", i);
            std::fflush(stdout);
        }
    }

    std::printf("parented %d victims, dead-at-take %d, create-fails %d\n",
                parentedCount, deadCount, createFailCount);
    std::printf("survived %d iterations\n", iterations);
}

int main(int argc, char **argv)
{
    // Shorten the incremental GC slices to ~1 ms. With the default ~5 ms budget
    // a whole mark phase of a small heap fits into one slice and the mutator
    // never runs mid-cycle; with 1 ms slices the mark phase yields constantly
    // and the window between MarkGlobalObject and DoSweep is wide open. (The
    // production crashes this reproduces happen with default settings under
    // real-world QML churn; see the accompanying write-up.)
    qputenv("QV4_GC_TIMELIMIT", "1");

    QCoreApplication app(argc, argv);
    qInstallMessageHandler(stderrHandler);

    const QString scenario = argc > 1 ? QString::fromLatin1(argv[1]) : QStringLiteral("intern");
    const int iterations = argc > 2 ? std::atoi(argv[2]) : 100000;
    const int preludeStrings = argc > 3 ? std::atoi(argv[3]) : 300000;

    QQmlEngine engine;
    growPrelude(engine, preludeStrings);

    if (scenario == QLatin1String("weak"))
        runWeakScenario(engine, iterations);
    else
        runInternScenario(engine, iterations);
    return 0;
}
