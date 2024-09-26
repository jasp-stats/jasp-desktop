#ifndef PDFDEFINITION_H
#define PDFDEFINITION_H
#include "enumutilities.h"

DECLARE_ENUM(pdfOrientation, portrait = 0, landscape); // Cf https://doc.qt.io/qt-6/qpagelayout.html#Orientation-enum
DECLARE_ENUM(pdfPageSize, letter = 0, legal, executive, A0, A1, A2, A3, A4, A5, A6); // Cf https://doc.qt.io/qt-6/qpagesize.html#PageSizeId-enum

#endif // PDFDEFINITION_H
