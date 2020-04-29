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
	Q_PROPERTY( bool			debug				READ debug				WRITE setDebug				NOTIFY debugChanged					)
	Q_PROPERTY( bool			parentDebug			READ parentDebug									NOTIFY parentDebugChanged			)
	Q_PROPERTY( bool			focusOnTab			READ focusOnTab			WRITE setFocusOnTab			NOTIFY focusOnTabChanged			)
	Q_PROPERTY( bool			hasError			READ hasError			WRITE setHasError			NOTIFY hasErrorChanged				)
	Q_PROPERTY( bool			hasWarning			READ hasWarning			WRITE setHasWarning			NOTIFY hasWarningChanged			)
	Q_PROPERTY( QQuickItem*		childControlsArea	READ childControlsArea	WRITE setChildControlsArea										)
	Q_PROPERTY( QQuickItem*		section				READ section			WRITE setSection												)
	Q_PROPERTY( QQuickItem*		parentListView		READ parentListView									NOTIFY parentListViewChanged		)
	Q_PROPERTY( QQuickItem*		innerControl		READ innerControl		WRITE setInnerControl		NOTIFY innerControlChanged			)
	Q_PROPERTY( QQuickItem*		background			READ background			WRITE setBackground			NOTIFY backgroundChanged			)

	Q_PROPERTY( QQmlListProperty<QQmlComponent> rowComponents READ rowComponents)
	Q_PROPERTY( QQmlComponent * rowComponent		READ rowComponent		WRITE setRowComponent		NOTIFY rowComponentChanged			)
	Q_PROPERTY( bool runAnalysisWhenOptionChanged	READ runAnalysisWhenOptionChanged	WRITE setRunAnalysisWhenOptionChanged	NOTIFY runAnalysisWhenOptionChangedChanged)


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
		, GroupBox
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
	bool			debug()					const	{ return _debug;				}
	bool			parentDebug()			const	{ return _parentDebug;			}
	bool			hasError()				const	{ return _hasError;				}
	bool			hasWarning()			const	{ return _hasWarning;			}
	bool			focusOnTab()			const	{ return activeFocusOnTab();	}
	AnalysisForm*	form()					const	{ return _form;					}
	QQuickItem*		childControlsArea()		const	{ return _childControlsArea;	}
	QQuickItem*		parentListView()		const	{ return _parentListView;		}
	QString			parentListViewKey()		const	{ return _parentListViewKey;	}
	QQuickItem*		section()				const	{ return _section;				}
	QQuickItem*		innerControl()			const	{ return _innerControl;				}
	QQuickItem*		background()			const	{ return _background;				}
	bool			runAnalysisWhenOptionChanged()	const	{ return _runAnalysisWhenOptionChanged; }


	void	setControlType(ControlType controlType)				{ _controlType = controlType; }
	void	setChildControlsArea(QQuickItem* childControlsArea)	{ _childControlsArea = childControlsArea; }
	void	setSection(QQuickItem* section)						{ _section = section; }
	void	setFocusOnTab(bool focus);
	void	setHasError(bool hasError);
	void	setHasWarning(bool hasWarning);
	void	setRunAnalysisWhenOptionChanged(bool change);
	void	setDebug(bool debug);
	void	setParentDebug(bool parentDebug);


	GENERIC_SET_FUNCTION(Name			, _name			, nameChanged			, QString		)
	GENERIC_SET_FUNCTION(IsBound		, _isBound		, isBoundChanged		, bool			)
	GENERIC_SET_FUNCTION(InnerControl	, _innerControl	, innerControlChanged	, QQuickItem*	)
	GENERIC_SET_FUNCTION(Background		, _background	, backgroundChanged		, QQuickItem*	)

	JASPControlWrapper*				getWrapper() const { return _wrapper; }

	void							setRowComponent(QQmlComponent * newRowComponent);
	QQmlListProperty<QQmlComponent>	rowComponents();
	void							appendRowComponent(QQmlComponent *);
	int								rowComponentsCount() const;
	QQmlComponent*					rowComponent(int) const;
	void							clearRowComponents();

	QQmlComponent*					rowComponent()			const	{ return _rowComponents.length() > 0 ? _rowComponents.at(0) : nullptr;	}
	QList<QQmlComponent*> &			getRowComponents()				{ return _rowComponents; }

	void							setOptionBlockSignal(bool blockSignal);

	Q_INVOKABLE	void				addControlError(QString message);
	Q_INVOKABLE void				addControlErrorTemporary(QString message);
	Q_INVOKABLE	void				addControlWarning(QString message);
	Q_INVOKABLE void				addControlWarningTemporary(QString message);
	Q_INVOKABLE void				clearControlError();

signals:
	void nameChanged();
	void isBoundChanged();
	void debugChanged();
	void parentDebugChanged();
	void hasErrorChanged();
	void hasWarningChanged();
	void focusOnTabChanged();
	void parentListViewChanged();
	void rowComponentChanged();
	void runAnalysisWhenOptionChangedChanged();
	void innerControlChanged();
	void backgroundChanged();

protected:
	void componentComplete() override;

	void _setType();

protected:
	ControlType			_controlType;
	QString				_name;
	bool				_isBound				= true;
	bool				_debug					= false;
	bool				_parentDebug			= false;
	bool				_hasError				= false;
	bool				_hasWarning				= false;
	JASPControlWrapper*	_wrapper				= nullptr;
	QQuickItem*			_parentListView			= nullptr;
	QString				_parentListViewKey;
	AnalysisForm*		_form					= nullptr;
	QQuickItem*			_childControlsArea		= nullptr;
	QQuickItem*			_section				= nullptr;
	QQuickItem*			_innerControl			= nullptr;
	QQuickItem*			_background				= nullptr;

	bool				_runAnalysisWhenOptionChanged = true;

	static void				appendRowComponent(QQmlListProperty<QQmlComponent>*, QQmlComponent*);
	static int				rowComponentsCount(QQmlListProperty<QQmlComponent>*);
	static QQmlComponent*	rowComponent(QQmlListProperty<QQmlComponent>*, int);
	static void				clearRowComponents(QQmlListProperty<QQmlComponent>*);

	QList<QQmlComponent*>	_rowComponents;


	static QList<JASPControlBase*>	getChildJASPControls(QQuickItem* item);
			void					setParentDebugToChildren(bool debug);
			void					setRunAnalysisWhenOptionChangedToChildren(bool change);

};


#endif // JASPCONTROLBASE_H
