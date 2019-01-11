#ifndef ANALYSISQMLDEFINES_H
#define ANALYSISQMLDEFINES_H

#include "enumutilities.h"

DECLARE_ENUM(qmlControlType, JASPControl, CheckBox, Switch, TextField, RadioButtonGroup, VariablesListView, ComboBox, FactorsList, TableView, Slider, TextArea, Button);
DECLARE_ENUM(qmlListViewType, AssignedVariables, AssignedPairs, Interaction, AvailableVariables, MeasuresCells, Layers);
DECLARE_ENUM(qmlDropMode, None, Replace, Insert)

#endif // ANALYSISQMLDEFINES_H
