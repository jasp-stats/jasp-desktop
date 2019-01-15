#ifndef ResourceButtons_H
#define ResourceButtons_H

#include <QAbstractListModel>
#include <map>
#include <set>



class ResourceButtons : public QAbstractListModel
{
	Q_OBJECT
public:
	//["Recent Files", "Current File", "Computer", "OSF", "Data Library"]
	enum ButtonType {RecentFiles, CurrentFile, Computer, OSF, DataLibrary};

	struct DataRow { ButtonType button; QString name; bool visible; QString qml; };

	enum ActionRoles { NameRole = Qt::UserRole + 1, TypeRole, VisibleRole, QmlRole };

	explicit ResourceButtons(QObject *parent = nullptr);

	//AbstractListModel functions
	int						rowCount(const QModelIndex & = QModelIndex())						const	override { return int(_data.size()); }
	QVariant				data(const QModelIndex &index, int role = Qt::DisplayRole)			const	override;
	bool					setData(const QModelIndex &index, const QVariant &value, int role)			override;
	QHash<int, QByteArray>	roleNames()															const	override;

	void					setOnlyTheseButtonsVisible(std::set<ButtonType> buttons = {});

signals:
	Q_INVOKABLE void clicked(int buttonType);

public slots:
	void setVisible(ResourceButtons::ButtonType button, bool visibility);

private:
	std::vector<DataRow>	_data;
	std::map<ButtonType, size_t>		_buttonToIndex;
};

#endif // ResourceButtons_H
