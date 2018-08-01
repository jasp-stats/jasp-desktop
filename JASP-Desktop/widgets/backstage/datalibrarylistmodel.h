#ifndef DATALIBRARYLISTMODEL_H
#define DATALIBRARYLISTMODEL_H

#include <QAbstractListModel>
#include "fsbmexamples.h"
#include "datalibrarybreadcrumbsmodel.h"
#include "data/fileevent.h"

class DataLibraryListModel : public QAbstractListModel
{
	Q_OBJECT

public:
	explicit DataLibraryListModel(QObject *parent = nullptr);
	void setDataLibraryBreadCrumbsModel (DataLibraryBreadCrumbsModel *dataLibraryBreadCrumbsMOdel);

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

signals:
	void openFile(FileEvent *event);

public slots:
	void changePath(const QString& name, const QString& path);
	void changePath(const int& index);
	
	void openFile(const QString& path);

private:
	FSBMExamples *_fsbmExampleModel;
	DataLibraryBreadCrumbsModel *_dataLibraryBreadCrumbsModel;
	QHash<int, QString> _iconsources;

};

#endif // DATALIBRARYLISTMODEL_H
