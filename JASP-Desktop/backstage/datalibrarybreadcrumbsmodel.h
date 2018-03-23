#ifndef DATALIBRARYBREADCRUMBSMODEL_H
#define DATALIBRARYBREADCRUMBSMODEL_H

#include <QAbstractListModel>

class DataLibraryBreadCrumbsModel : public QAbstractListModel
{
	Q_OBJECT
	
public:
	enum Roles
    {
        NameRole = Qt::UserRole
    };
	
	explicit DataLibraryBreadCrumbsModel(QObject *parent = nullptr);
	void setSeperator(const QChar &separator);
		
	// Basic functionality:
	int rowCount(const QModelIndex &parent = QModelIndex()) const override;
	
	QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const override;
	
	// Editable:
	bool setData(const QModelIndex &index, const QVariant &value,
				 int role = Qt::EditRole) override;
	
	Qt::ItemFlags flags(const QModelIndex& index) const override;
	
	QHash<int, QByteArray> roleNames() const override;
	
public slots:
	void appendCrumb(QString crumb);
	QString changeCrumb(QString crum);
	void removeLastCrumb();
	bool removeCrumbsAfterIndex(int index);
	void handleCrumbClicked(int index);
	
private:
	QStringList _pathList;
	QChar _separator = QChar('/');	
};

#endif // DATALIBRARYBREADCRUMBSMODEL_H
