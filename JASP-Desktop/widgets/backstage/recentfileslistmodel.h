#ifndef RECENTFILESLISTMODEL_H
#define RECENTFILESLISTMODEL_H

#include <QAbstractListModel>
#include "data/fileevent.h"
#include "fsbmrecentfiles.h"

class RecentFilesListModel : public QAbstractListModel
{
	Q_OBJECT

public:
	explicit RecentFilesListModel(QObject *parent = nullptr);

	enum
	{
		NameRole = Qt::UserRole,
		PathRole,
		FolderRole,
		TypeRole,
		IconSourceRole
	};

	// Basic functionality:
	int rowCount(const QModelIndex &parent = QModelIndex()) const override;
	QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const override;

	// Editable:
	bool setData(const QModelIndex &index, const QVariant &value,
				 int role = Qt::EditRole) override;
	Qt::ItemFlags flags(const QModelIndex& index) const override;
	virtual QHash<int, QByteArray> roleNames() const override;
	
	void addRecentFilePath(const QString &newpath);

signals:
	void openFile(FileEvent *event);

public slots:
	void openFile(const QString& path);

private:
	FSBMRecentFiles *_fsbmRecentFiles;
	QHash<int, QString> _iconsources;

};

#endif // RECENTFILESLISTMODEL_H
