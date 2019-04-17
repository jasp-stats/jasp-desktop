#ifndef ResourceButtons_H
#define ResourceButtons_H

#include <QAbstractListModel>
#include <map>
#include <set>



class ResourceButtons : public QAbstractListModel
{
	Q_OBJECT
	Q_PROPERTY(QString currentQML READ currentQML WRITE setCurrentQML NOTIFY currentQMLChanged)

public:
	//["Recent Files", "Current File", "Computer", "OSF", "Data Library"]
	enum ButtonType {RecentFiles, CurrentFile, Computer, OSF, DataLibrary, PrefsData, PrefsResults, PrefsAdvanced};

	struct DataRow { ButtonType button; QString name; bool visible; QString qml; bool enabled; };

	enum ActionRoles { NameRole = Qt::UserRole + 1, TypeRole, VisibleRole, QmlRole, EnabledRole };

	explicit ResourceButtons(QObject *parent = nullptr);

	//AbstractListModel functions
	int						rowCount(const QModelIndex & = QModelIndex())						const	override { return int(_data.size()); }
	QVariant				data(const QModelIndex &index, int role = Qt::DisplayRole)			const	override;
	bool					setData(const QModelIndex &index, const QVariant &value, int role)			override;
	QHash<int, QByteArray>	roleNames()															const	override;

	void					setOnlyTheseButtonsVisible(std::set<ButtonType> buttons = {});
	void					setButtonEnabled(ButtonType button, bool enabled);

	QString					qml(ResourceButtons::ButtonType button);

	QString					currentQML() const	{ return _currentQML; }

signals:
	Q_INVOKABLE void clicked(int buttonType);

	void currentQMLChanged(QString currentQML);

public slots:
	void setVisible(ResourceButtons::ButtonType button, bool visibility);
	void setCurrentQML(QString currentQML);
	void clickedHandler(int buttonType) { setCurrentQML(qml(static_cast<ButtonType>(buttonType))); }

private:
	std::vector<DataRow>			_data;
	std::map<ButtonType, size_t>	_buttonToIndex;
	QString							_currentQML;
};

#endif // ResourceButtons_H
