#ifndef COMPUTERLISTMODEL_H
#define COMPUTERLISTMODEL_H

#include <QAbstractListModel>
#include "data/fileevent.h"
#include "fsbmrecentfolders.h"

class ComputerListModel : public QAbstractListModel
{
	Q_OBJECT
	
	enum
	{
		NameRole = Qt::UserRole,
		PathRole,
		FolderRole,
		TypeRole,
		IconSourceRole
	};
	
public:
	explicit ComputerListModel(QObject *parent = nullptr);
	
	// Basic functionality:
	int rowCount(const QModelIndex &parent = QModelIndex()) const override;
	
	QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const override;
	virtual QHash<int, QByteArray> roleNames() const override;
	
	QString getMostRecent();
	void addRecentFolder(const QString &newpath);
	void refresh();
					
private:
	FSBMRecentFolders *_fsbmRecentFoilders;
	QHash<int, QString> _iconsources;
};

#endif // COMPUTERLISTMODEL_H
