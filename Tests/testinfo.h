#ifndef TESTINFO_H
#define TESTINFO_H

#include <QDir>

#define TO_STR2(x) #x
#define TO_STR(x) TO_STR2(x)

inline QDir _testLibrary()
{
	static const char *testLibraryDir = TO_STR(TESTLIBRARY_DIR);

	QString altDir = qgetenv("TESTLIBRARY_DIR");
	return altDir.isEmpty() ? QString(testLibraryDir) : altDir;
}

#endif // TESTINFO_H
