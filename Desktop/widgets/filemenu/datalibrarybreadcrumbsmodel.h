#ifndef DATALIBRARYBREADCRUMBSLISTMODEL_H
#define DATALIBRARYBREADCRUMBSLISTMODEL_H

#include <QAbstractListModel>

class DataLibraryBreadCrumbsListModel : public QAbstractListModel
{
	Q_OBJECT
	
public:
	enum Roles
    {
        NameRole = Qt::UserRole
    };
	
	explicit DataLibraryBreadCrumbsListModel(QObject *parent, const QChar sep);
	void setSeparator(const QChar &separator);
		
	// Basic functionality:
	int rowCount(const QModelIndex &parent = QModelIndex()) const override;
	
	QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const override;
	
	// Editable:
	bool setData(const QModelIndex &index, const QVariant &value,
				 int role = Qt::EditRole) override;	
	Qt::ItemFlags flags(const QModelIndex& index) const override;	
	QHash<int, QByteArray> roleNames() const override;
	
	void appendCrumb(const QString &crumbname, const QString &path);
	QString switchCrumb(const int &index);
	void refresh();
	
private:
	bool removeCrumbsAfterIndex(int index);
	QString getTranslaterRootElement();
	
public slots:
	void indexChanged(const int &index);
	
signals:
	void crumbIndexChanged(const int &index);
	
private:
	QStringList _crumbNameList;
	QStringList _physicalPathList;
	QChar _separator = QChar('/');

};

#endif // DATALIBRARYBREADCRUMBSLISTMODEL_H
