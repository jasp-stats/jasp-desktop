#include "AutomatedTests.h"
#include <QDebug>

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);

    int failures = AutomatedTests::run(argc, argv);

    qDebug() << "--------------------------------------------------------------------------------------";

    if (failures == 0)
    {
      qDebug() << "ALL TESTS PASSED";
    }
    else
    {
      qDebug() << failures << " TESTS FAILED!";
    }

    qDebug() << "--------------------------------------------------------------------------------------";

    return 0;
}

