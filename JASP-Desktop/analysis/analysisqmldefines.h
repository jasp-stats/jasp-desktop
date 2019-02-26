#ifndef ANALYSISQMLDEFINES_H
#define ANALYSISQMLDEFINES_H

#include "enumutilities.h"

DECLARE_ENUM(qmlControlType, JASPControl, CheckBox, Switch, TextField, RadioButtonGroup, VariablesListView, ComboBox, RepeatedMeasuresFactorsList, TableView, Slider, TextArea, Button);
DECLARE_ENUM(qmlListViewType, AssignedVariables, Pairs, Interaction, AvailableVariables, RepeatedMeasures, Layers, AvailableInteractons);
DECLARE_ENUM(qmlDropMode, None, Replace, Insert)

#endif // ANALYSISQMLDEFINES_H
