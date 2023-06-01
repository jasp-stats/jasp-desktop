#ifndef FILEMENUBASICLISTMODEL_H
#define FILEMENUBASICLISTMODEL_H

#include <QAbstractListModel>
#include <QMutex>

#include "datalibraryfilesystem.h"
#include "datalibrarybreadcrumbsmodel.h"
#include "data/fileevent.h"
#include "filemenulistitem.h"

class FileMenuBasicListModel : public QAbstractListModel
{
	Q_OBJECT

public:
	explicit FileMenuBasicListModel(QObject *parent, FileSystem * model);
	virtual ~FileMenuBasicListModel() {}

	int						rowCount(const QModelIndex &parent = QModelIndex())									const	override;
	QVariant				data(const QModelIndex &index, int role = Qt::DisplayRole)							const	override;
	bool					setData(const QModelIndex &index, const QVariant &value, int role = Qt::EditRole)			override;
	QHash<int, QByteArray>	roleNames()																			const	override { return FileMenuListItemTypeRoleNames; }
	Qt::ItemFlags			flags(const QModelIndex& index)														const	override;


public slots:
	virtual void changePath(const QString& name, const QString& path);
	virtual void changePathCrumbIndex(const int& index);
	virtual void openFile(const QString& path);
	virtual void saveFile(const QString& path);

	void resetPath() { changePathCrumbIndex(0); }
	bool mayOpen();

protected:
	FileSystem *_model = nullptr;
	bool			_openFileWhenClicked = true;
	static QMutex	_opening;
	void			resetOpening();
};

#endif // FILEMENUBASICLISTMODEL_H
