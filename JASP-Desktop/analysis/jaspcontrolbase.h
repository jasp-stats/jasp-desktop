#ifndef JASPCONTROLBASE_H
#define JASPCONTROLBASE_H

#include <QQuickItem>

#include "utilities/qutils.h"

class AnalysisForm;
class JASPControlWrapper;

class JASPControlBase : public QQuickItem
{
	Q_OBJECT

private:
	Q_PROPERTY( ControlType		controlType			READ controlType		WRITE setControlType											)
	Q_PROPERTY( QString			name				READ name				WRITE setName				NOTIFY nameChanged					)
	Q_PROPERTY( bool			isBound				READ isBound			WRITE setIsBound			NOTIFY isBoundChanged				)
	Q_PROPERTY( bool			focusOnTab			READ focusOnTab			WRITE setFocusOnTab			NOTIFY focusOnTabChanged			)
	Q_PROPERTY( bool			hasError			READ hasError			WRITE setHasError			NOTIFY hasErrorChanged				)
	Q_PROPERTY( bool			hasWarning			READ hasWarning			WRITE setHasWarning			NOTIFY hasWarningChanged			)
	Q_PROPERTY( QVariant		childControlsArea	READ childControlsArea	WRITE setChildControlsArea										)
	Q_PROPERTY( QQuickItem*		section				READ section			WRITE setSection												)
	Q_PROPERTY( QQuickItem*		parentListView		READ parentListView									NOTIFY parentListViewChanged		)
	Q_PROPERTY( QQmlListProperty<QQmlComponent> rowComponents READ rowComponents)
	Q_PROPERTY( QQmlComponent * rowComponent		READ rowComponent		WRITE setRowComponent		NOTIFY rowComponentChanged			)


public:
	enum class ControlType {
		  JASPControl
		, Expander
		, CheckBox
		, Switch
		, TextField
		, RadioButton
		, RadioButtonGroup
		, VariablesListView
		, ComboBox
		, RepeatedMeasuresFactorsList
		, InputListView
		, TableView
		, Slider
		, TextArea
		, Button
		, FactorsForm
		, ComponentsList
	};

	// Be careful not to reuse a name in a enum type: in QML, they are mixed up with a 'JASP' prefix: JASP.DropNone or JASP.None
	enum class Inclusive	{ None = 0, MinMax, MinOnly, MaxOnly };
	enum class DropMode		{ DropNone = static_cast<int>(Inclusive::MaxOnly) + 1, DropInsert, DropReplace };
	enum class ListViewType { AssignedVariables = static_cast<int>(DropMode::DropReplace) + 1, Interaction, AvailableVariables, RepeatedMeasures, Layers, AvailableInteraction };
	enum class AssignType	{ AssignDefault = static_cast<int>(ListViewType::AvailableInteraction) + 1, AssignCross, AssignMainEffects, AssignInteraction, AssignAll2Way, AssignAll3Way, AssignAll4Way, AssignAll5Way };


	Q_ENUM(ControlType)
	Q_ENUM(Inclusive)
	Q_ENUM(DropMode)
	Q_ENUM(ListViewType)
	Q_ENUM(AssignType)

	JASPControlBase(QQuickItem *parent = nullptr);

	ControlType		controlType()			const	{ return _controlType;			}
	const QString&	name()					const	{ return _name;					}
	bool			isBound()				const	{ return _isBound;				}
	bool			hasError()				const	{ return _hasError;				}
	bool			hasWarning()			const	{ return _hasWarning;			}
	bool			focusOnTab()			const	{ return activeFocusOnTab();	}
	AnalysisForm*	form()					const	{ return _form;					}
	const QVariant&	childControlsArea()		const	{ return _childControlsArea;	}
	QQuickItem*		parentListView()		const	{ return _parentListView;		}
	QString			parentListViewKey()		const	{ return _parentListViewKey;	}
	QQuickItem*		section()				const	{ return _section;				}


	void	setControlType(ControlType controlType)				{ _controlType = controlType; }
	void	setChildControlsArea(QVariant childControlsArea)	{ _childControlsArea = childControlsArea; }
	void	setSection(QQuickItem* section)						{ _section = section; }
	void	setFocusOnTab(bool focus);
	void	setHasError(bool hasError);
	void	setHasWarning(bool hasWarning);


	GENERIC_SET_FUNCTION(Name		, _name			, nameChanged		, QString	)
	GENERIC_SET_FUNCTION(IsBound	, _isBound		, isBoundChanged	, bool		)

	JASPControlWrapper*				getWrapper() const { return _wrapper; }

	void							setRowComponent(QQmlComponent * newRowComponent);
	QQmlListProperty<QQmlComponent>	rowComponents();
	void							appendRowComponent(QQmlComponent *);
	int								rowComponentsCount() const;
	QQmlComponent*					rowComponent(int) const;
	void							clearRowComponents();

	QQmlComponent*					rowComponent()			const	{ return _rowComponents.length() > 0 ? _rowComponents.at(0) : nullptr;	}
	QList<QQmlComponent*> &			getRowComponents()				{ return _rowComponents; }

	Q_INVOKABLE	void				addControlError(QString message);
	Q_INVOKABLE void				addControlErrorTemporary(QString message);
	Q_INVOKABLE	void				addControlWarning(QString message);
	Q_INVOKABLE void				addControlWarningTemporary(QString message);
	Q_INVOKABLE void				clearControlError();

signals:
	void nameChanged();
	void isBoundChanged();
	void hasErrorChanged();
	void hasWarningChanged();
	void focusOnTabChanged();
	void parentListViewChanged();
	void rowComponentChanged();

protected:
	void componentComplete() override;

	void _setType();

protected:
	ControlType			_controlType;
	QString				_name;
	bool				_isBound				= true;
	bool				_hasError				= false;
	bool				_hasWarning				= false;
	JASPControlWrapper*	_wrapper				= nullptr;
	QQuickItem*			_parentListView			= nullptr;
	QString				_parentListViewKey;
	AnalysisForm*		_form					= nullptr;
	QVariant			_childControlsArea;
	QQuickItem*			_section				= nullptr;

	static void				appendRowComponent(QQmlListProperty<QQmlComponent>*, QQmlComponent*);
	static int				rowComponentsCount(QQmlListProperty<QQmlComponent>*);
	static QQmlComponent*	rowComponent(QQmlListProperty<QQmlComponent>*, int);
	static void				clearRowComponents(QQmlListProperty<QQmlComponent>*);

	QList<QQmlComponent*>	_rowComponents;
};


#endif // JASPCONTROLBASE_H
