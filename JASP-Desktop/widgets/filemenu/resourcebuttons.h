#ifndef ResourceButtons_H
#define ResourceButtons_H

#include <QAbstractListModel>
#include <map>
#include <set>



class ResourceButtons : public QAbstractListModel
{
	Q_OBJECT
	Q_PROPERTY(QString		currentQML		READ currentQML		WRITE setCurrentQML		NOTIFY currentQMLChanged	)
	Q_PROPERTY(ButtonType	selectedButton	READ selectedButton WRITE setSelectedButton	NOTIFY selectedButtonChanged)

public:
	//["Recent Files", "Current File", "Computer", "OSF", "Data Library"]
	enum ButtonType {None, RecentFiles, CurrentFile, Computer, OSF, DataLibrary, PrefsData, PrefsResults, PrefsUI, PrefsAdvanced};
	Q_ENUM(ButtonType)

	struct DataRow { ButtonType button; QString name; bool visible; QString qml; bool enabled; };

	enum ActionRoles { NameRole = Qt::UserRole + 1, TypeRole, VisibleRole, QmlRole, EnabledRole, SelectedRole };

	explicit ResourceButtons(QObject *parent = nullptr);

	//AbstractListModel functions
	int						rowCount(const QModelIndex & = QModelIndex())						const	override { return int(_data.size()); }
	QVariant				data(const QModelIndex &index, int role = Qt::DisplayRole)			const	override;
	bool					setData(const QModelIndex &index, const QVariant &value, int role)			override;
	QHash<int, QByteArray>	roleNames()															const	override;

	void					setOnlyTheseButtonsVisible(std::set<ButtonType> buttons = {});
	void					setButtonEnabled(ButtonType button, bool enabled);

	QString					qml(ResourceButtons::ButtonType button);

	QString					currentQML()		const { return _currentQML;		}
	ButtonType				selectedButton()	const { return _selectedButton;	}

	Q_INVOKABLE void		selectFirstButtonIfNoneSelected();
	Q_INVOKABLE	void		selectButtonUp();
	Q_INVOKABLE	void		selectButtonDown();
	void refresh();

signals:
	void currentQMLChanged(QString currentQML);
	void selectedButtonChanged(ButtonType selectedButton);

public slots:
	void setVisible(ResourceButtons::ButtonType button, bool visibility);
	void setCurrentQML(QString currentQML);
	void setSelectedButton(ButtonType selectedButton);

private:
	void loadButtonData(std::vector<DataRow> & data);
	std::vector<DataRow>			_data;
	std::map<ButtonType, size_t>	_buttonToIndex;
	QString							_currentQML;
	ButtonType						_selectedButton = None;
};

#endif // ResourceButtons_H
