#ifndef ANALYSISQMLDEFINES_H
#define ANALYSISQMLDEFINES_H

#include "enumutilities.h"

DECLARE_ENUM(qmlControlType, JASPControl, CheckBox, Switch, TextField, ButtonGroup, VariablesTable, DraggableListView, ComboBox, FactorsList);
DECLARE_ENUM(qmlListViewType, assignedVariables, assignedPairs, assignedAnova, availableVariables, measuresCells);
DECLARE_ENUM(qmlDropMode, None, Replace, Insert)

#endif // ANALYSISQMLDEFINES_H
