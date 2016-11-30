//
// Copyright (C) 2016 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//


// Script runs all the qt tests in the test folder
#ifndef AUTOMATEDTESTS_H
#define AUTOMATEDTESTS_H

#ifdef __APPLE__
#define TESTFILE_FOLDER "../Resources/TestFiles/"
#else
#define TESTFILE_FOLDER "Resources/TestFiles/"
#endif

#include <QList>
#include <QSharedPointer>
#include <QString>
#include <QtTest/QtTest>
#include <QtWidgets>

namespace AutomatedTests
{
	typedef QList<QObject*> TestList;

	inline TestList& testList()
	{
		static TestList list;
		return list;
	}

	inline bool findObject(QObject* obj)
	{
		TestList& list = testList();
		if (list.contains(obj))
		{
			return true;
		}

		foreach (QObject* test, list)
		{
			if (test->objectName() == obj->objectName())
			{
				return true;
			}
		}
		return false;
	}

	inline void addTest(QObject* obj)
	{
		TestList& list = testList();
		if (!findObject(obj))
		{
			list.append(obj);
		}
	}

	inline int run(int argc, char *argv[])
	{
		int ret = 0;

		foreach (QObject* test, testList())
		{
			ret += QTest::qExec(test, argc, argv);
		}

		return ret;
	}
}

template <class T>
class Test
{
	public:
	QSharedPointer<T> child;

	Test(const QString& name): child(new T)
	{
		child->setObjectName(name);
		AutomatedTests::addTest(child.data());
	}
};

#define DECLARE_TEST(className) static Test<className> t(#className);

#define TEST_MAIN \
int main(int argc, char *argv[]) \
{ \
	return AutomatedTests::run(argc, argv); \
}

#endif // AUTOMATEDTESTS_H
