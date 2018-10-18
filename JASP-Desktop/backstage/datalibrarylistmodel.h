#ifndef DATALIBRARYLISTMODEL_H
#define DATALIBRARYLISTMODEL_H

#include <QAbstractListModel>
#include "fsbmdatalibrary.h"
#include "datalibrarybreadcrumbsmodel.h"
#include "fileevent.h"

class DataLibraryListModel : public QAbstractListModel
{
	Q_OBJECT

public:
	explicit DataLibraryListModel(QObject *parent = nullptr);
	
	enum
	{
		NameRole = Qt::UserRole,
		PathRole,
		DescriptionRole,
		TypeRole,
		AssociatedDataFileRole,
		IconSourceRole,
		DataIconSourceRole,
		DirRole
	};

	// Basic functionality:
	int rowCount(const QModelIndex &parent = QModelIndex()) const override;

	QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const override;

	// Editable:
	bool setData(const QModelIndex &index, const QVariant &value,
				 int role = Qt::EditRole) override;


	Qt::ItemFlags flags(const QModelIndex& index) const override;

	virtual QHash<int, QByteArray> roleNames() const override;
	
	void setBreadCrumbsListModel (DataLibraryBreadCrumbsListModel *dataLibraryBreadCrumbsModel);

signals:
	void openFile(FileEvent *event);

public slots:
	void changePath(const QString& name, const QString& path);
	void changePath(const int& index);
	
	void openFile(const QString& path);

private:
	FSBMDataLibrary *_fsbmDataLibrary;
	DataLibraryBreadCrumbsListModel *_dataLibraryBreadCrumbsListModel;
	QHash<int, QString> _iconsources;

};

#endif // DATALIBRARYLISTMODEL_H
