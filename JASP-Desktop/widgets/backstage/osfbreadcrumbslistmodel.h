#ifndef OSFBREADCRUMBSLISTMODEL_H
#define OSFBREADCRUMBSLISTMODEL_H

#include <QAbstractListModel>

class OSFBreadCrumbsListModel : public QAbstractListModel
{
	Q_OBJECT
	
public:
	enum Roles
    {
        NameRole = Qt::UserRole
    };
	
	explicit OSFBreadCrumbsListModel(QObject *parent, const QChar separator);
	void setSeparator(const QChar &separator);
	
	// Basic functionality:
	int rowCount(const QModelIndex &parent = QModelIndex()) const override;
	
	QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const override;	
	QHash<int, QByteArray> roleNames() const override;
	
	void appendCrumb(const QString &crumbname, const QString &path);
	QString switchCrumb(const int &index);
	
private:
	bool removeCrumbsAfterIndex(int index);

public slots:
	void indexChanged(const int &index);

signals:
	void crumbIndexChanged(const int &index);
	
private:
	QStringList _crumbNameList;
	QStringList _physicalPathList;
	QChar _separator = QChar('/');	
};

#endif // OSFBREADCRUMBSLISTMODEL_H
