#ifndef RibbonModelUncommon_H
#define RibbonModelUncommon_H

#include <QSortFilterProxyModel>
#include "ribbonmodel.h"

///
/// This filters the RibbonButtons made available by RibbonModel.
/// It passes through all actual modules, common (core) ones included, as well as special buttons that may be toggled (such as the R-console).
/// This is used by the modules-menu to allow users to turn modules on and off.
///
class RibbonModelUncommon : public QSortFilterProxyModel
{
	Q_OBJECT

public:
	RibbonModelUncommon(QObject * parent = nullptr, RibbonModel * ribbonModel = nullptr);

	void setRibbonModel(RibbonModel * ribbonModel);

	bool filterAcceptsRow(int source_row, const QModelIndex &source_parent) const override;

	Q_INVOKABLE void setModuleEnabled(int filteredRow, bool checked);
	Q_INVOKABLE void moveModule(int from, int to); //Takes filtered rows and forwards them as source rows to RibbonModel::moveModule

private:
	RibbonModel		*_ribbonModel			= nullptr;
};

#endif // RibbonModelUncommon_H
