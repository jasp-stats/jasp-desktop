#ifndef CURRENTFILELISTMODEL_H
#define CURRENTFILELISTMODEL_H

#include <QAbstractListModel>
#include "fsbmcurrentfile.h"
#include "fileevent.h"

class CurrentFileListModel : public QAbstractListModel
{
	Q_OBJECT
	
public:
	explicit CurrentFileListModel(QObject *parent = nullptr);
	
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
	
	//Special
	FSBMCurrentFile* getCurrentFileFSBModel();
	void setCurrentFilePath(const QString &newcurrent);
	
signals:
	void syncFile(FileEvent *event);

public slots:
	void syncFile(const QString& path);	
	
	
private:
	FSBMCurrentFile *_fsbmCurrentFile;
	QHash<int, QString> _iconsources;
};

#endif // CURRENTFILELISTMODEL_H
