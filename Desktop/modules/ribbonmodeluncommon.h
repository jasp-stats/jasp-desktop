#ifndef RibbonModelUncommon_H
#define RibbonModelUncommon_H

#include <QSortFilterProxyModel>
#include "ribbonmodel.h"

///
/// This filters the RibbonButtons made available by RibbonModel.
/// It only passes through those bundled modules not in Common or those installed manually by the user
/// This is used by the modules-menu to allow users to turn modules on and off, because those in Common are always enabled
class RibbonModelUncommon : public QSortFilterProxyModel
{
	Q_OBJECT

public:
	RibbonModelUncommon(QObject * parent = nullptr, RibbonModel * ribbonModel = nullptr);

	void setRibbonModel(RibbonModel * ribbonModel);

	bool filterAcceptsRow(int source_row, const QModelIndex &source_parent) const override;

	Q_INVOKABLE void setModuleEnabled(int filteredRow, bool checked);

private:
	RibbonModel		*_ribbonModel			= nullptr;
};

#endif // RibbonModelUncommon_H
