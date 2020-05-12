#ifndef JASPCONTROLBASE_H
#define JASPCONTROLBASE_H

#include <QQuickItem>

#include "utilities/qutils.h"


class AnalysisForm;
class JASPControlWrapper;

class JASPControlBase : public QQuickItem
{
	Q_OBJECT

	typedef QQmlListProperty<QQmlComponent> QQmlComponents;

	Q_PROPERTY( ControlType			controlType			READ controlType		WRITE setControlType											)
	Q_PROPERTY( QString				name				READ name				WRITE setName				NOTIFY nameChanged					)
	Q_PROPERTY( QString				title				READ title				WRITE setTitle				NOTIFY titleChanged					) //Basically whatever a human sees on their screen when they look at this specific item.
	Q_PROPERTY( QString				info				READ info				WRITE setInfo				NOTIFY infoChanged					)
	Q_PROPERTY( QString				helpMD				READ helpMD											NOTIFY helpMDChanged				)
	Q_PROPERTY( bool				isBound				READ isBound			WRITE setIsBound			NOTIFY isBoundChanged				)
	Q_PROPERTY( bool				debug				READ debug				WRITE setDebug				NOTIFY debugChanged					)
	Q_PROPERTY( bool				parentDebug			READ parentDebug									NOTIFY parentDebugChanged			)
	Q_PROPERTY( bool				focusOnTab			READ focusOnTab			WRITE setFocusOnTab			NOTIFY focusOnTabChanged			)
	Q_PROPERTY( bool				hasError			READ hasError			WRITE setHasError			NOTIFY hasErrorChanged				)
	Q_PROPERTY( bool				hasWarning			READ hasWarning			WRITE setHasWarning			NOTIFY hasWarningChanged			)
	Q_PROPERTY( bool				runOnChange			READ runOnChange		WRITE setRunOnChange		NOTIFY runOnChangeChanged			)
	Q_PROPERTY( QQuickItem		*	childControlsArea	READ childControlsArea	WRITE setChildControlsArea										)
	Q_PROPERTY( QQuickItem		*	section				READ section			WRITE setSection												)
	Q_PROPERTY( QQuickItem		*	parentListView		READ parentListView									NOTIFY parentListViewChanged		)
	Q_PROPERTY( QQuickItem		*	innerControl		READ innerControl		WRITE setInnerControl		NOTIFY innerControlChanged			)
	Q_PROPERTY( QQuickItem		*	background			READ background			WRITE setBackground			NOTIFY backgroundChanged			)
	Q_PROPERTY( QQmlComponent	*	rowComponent		READ rowComponent		WRITE setRowComponent		NOTIFY rowComponentChanged			)
	Q_PROPERTY( QQmlComponents		rowComponents		READ rowComponents)


public:
	// Any addition here should also be added manually to ControlTypeToFriendlyString... I couldnt get this to work with DECLARE_ENUM...
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
		, TabView
	};

	// Be careful not to reuse a name in a enum type: in QML, they are mixed up with a 'JASP' prefix: JASP.DropNone or JASP.None
	enum class Inclusive	{ None				= 0,															MinMax, MinOnly, MaxOnly };
	enum class DropMode		{ DropNone			= static_cast<int>(Inclusive::MaxOnly)					+ 1,	DropInsert, DropReplace };
	enum class ListViewType { AssignedVariables = static_cast<int>(DropMode::DropReplace)				+ 1,	Interaction, AvailableVariables, RepeatedMeasures, Layers, AvailableInteraction };
	enum class AssignType	{ AssignDefault		= static_cast<int>(ListViewType::AvailableInteraction)	+ 1,	AssignCross, AssignMainEffects, AssignInteraction, AssignAll2Way, AssignAll3Way, AssignAll4Way, AssignAll5Way };


	Q_ENUM(ControlType)
	Q_ENUM(Inclusive)
	Q_ENUM(DropMode)
	Q_ENUM(ListViewType)
	Q_ENUM(AssignType)

	JASPControlBase(QQuickItem *parent = nullptr);

	ControlType		controlType()			const	{ return _controlType;			}
	const QString&	name()					const	{ return _name;					}
	QString			title()					const	{ return _title;				}
	QString			info()					const	{ return _info;					}
	QString			helpMD(int howDeep = 2)	const;
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
	QQuickItem*		innerControl()			const	{ return _innerControl;			}
	QQuickItem*		background()			const	{ return _background;			}
	bool			runOnChange()			const	{ return _runOnChange;			}


	JASPControlWrapper		*	getWrapper()				const { return _wrapper; }
	QQmlComponent			*	rowComponent()				const { return _rowComponents.length() > 0 ? _rowComponents.at(0) : nullptr;	}
	QQmlComponent			*	rowComponent(int)			const;
	QQmlComponents				rowComponents();
	int							rowComponentsCount()		const;
	void						setRowComponent(	QQmlComponent * newRowComponent);
	void						appendRowComponent(	QQmlComponent * newRowComponent);
	void						clearRowComponents();
	QList<QQmlComponent*>	&	getRowComponents()				{ return _rowComponents; }

	static QString					ControlTypeToFriendlyString(ControlType controlType);
	static QList<JASPControlBase*>	getChildJASPControls(const QQuickItem* item);

public slots:
	void	setControlType(			ControlType		controlType)		{ _controlType = controlType; }
	void	setChildControlsArea(	QQuickItem	*	childControlsArea);
	void	setSection(				QQuickItem	*	section)			{ _section = section; }
	void	setFocusOnTab(			bool focus);
	void	setHasError(			bool hasError);
	void	setHasWarning(			bool hasWarning);
	void	setRunOnChange(			bool change);
	void	setDebug(				bool debug);
	void	setParentDebug(			bool parentDebug);

	void	addControlError(			QString message);
	void	addControlErrorTemporary(	QString message);
	void	addControlWarning(			QString message);
	void	addControlWarningTemporary(	QString message);
	void	clearControlError();

	void	reconnectWithYourChildren();

	GENERIC_SET_FUNCTION(Name			, _name			, nameChanged			, QString		)
	GENERIC_SET_FUNCTION(Info			, _info			, infoChanged			, QString		)
	GENERIC_SET_FUNCTION(Title			, _title		, titleChanged			, QString		)
	GENERIC_SET_FUNCTION(IsBound		, _isBound		, isBoundChanged		, bool			)
	GENERIC_SET_FUNCTION(InnerControl	, _innerControl	, innerControlChanged	, QQuickItem*	)
	GENERIC_SET_FUNCTION(Background		, _background	, backgroundChanged		, QQuickItem*	)

signals:
	void setOptionBlockSignal(	bool blockSignal);
	void nameChanged();
	void isBoundChanged();
	void debugChanged();
	void parentDebugChanged();
	void hasErrorChanged();
	void hasWarningChanged();
	void focusOnTabChanged();
	void parentListViewChanged();
	void rowComponentChanged();
	void runOnChangeChanged();
	void innerControlChanged();
	void backgroundChanged();
	void valueChanged();
	void infoChanged();
	void titleChanged();
	void helpMDChanged();

protected:
			void					componentComplete() override;
			void					_setType();

	static	void					appendRowComponent(QQmlListProperty<QQmlComponent>*, QQmlComponent*);
	static	int						rowComponentsCount(QQmlListProperty<QQmlComponent>*);
	static	QQmlComponent*			rowComponent(QQmlListProperty<QQmlComponent>*, int);
	static	void					clearRowComponents(QQmlListProperty<QQmlComponent>*);


			void					setParentDebugToChildren(bool debug);
			void					setRunOnChangeToChildren(bool change);

protected:
	ControlType				_controlType;
	AnalysisForm*			_form					= nullptr;
	QString					_name,
							_info					= "",
							_title,
							_parentListViewKey;
	bool					_isBound				= true,
							_debug					= false,
							_parentDebug			= false,
							_hasError				= false,
							_hasWarning				= false,
							_runOnChange			= true;
	JASPControlWrapper	*	_wrapper				= nullptr;
	QQuickItem			*	_parentListView			= nullptr,
						*	_childControlsArea		= nullptr,
						*	_section				= nullptr,
						*	_innerControl			= nullptr,
						*	_background				= nullptr;
	QList<QQmlComponent*>	_rowComponents;
};


#endif // JASPCONTROLBASE_H
