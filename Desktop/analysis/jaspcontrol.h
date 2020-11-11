#ifndef JASPCONTROL_H
#define JASPCONTROL_H

#include <QQuickItem>
#include <QPropertyAnimation>

#include "utilities/qutils.h"


class AnalysisForm;
class JASPControlWrapper;

class JASPControl : public QQuickItem
{
	Q_OBJECT

	Q_PROPERTY( ControlType							controlType			READ controlType			WRITE setControlType											)
	Q_PROPERTY( QString								name				READ name					WRITE setName				NOTIFY nameChanged					)
	Q_PROPERTY( QString								title				READ title					WRITE setTitle				NOTIFY titleChanged					) //Basically whatever a human sees on their screen when they look at this specific item.
	Q_PROPERTY( QString								info				READ info					WRITE setInfo				NOTIFY infoChanged					)
	Q_PROPERTY( QString								toolTip				READ toolTip				WRITE setToolTip			NOTIFY toolTipChanged				)
	Q_PROPERTY( QString								helpMD				READ helpMD												NOTIFY helpMDChanged				)
	Q_PROPERTY( bool								isBound				READ isBound				WRITE setIsBound			NOTIFY isBoundChanged				)
	Q_PROPERTY( bool								indent				READ indent					WRITE setIndent				NOTIFY indentChanged				)
	Q_PROPERTY( bool								useControlMouseArea	READ useControlMouseArea	WRITE setUseControlMouseArea NOTIFY useControlMouseAreaChanged	)
	Q_PROPERTY( bool								isDependency		READ isDependency			WRITE setIsDependency		NOTIFY isDependencyChanged			)
	Q_PROPERTY( bool								debug				READ debug					WRITE setDebug				NOTIFY debugChanged					)
	Q_PROPERTY( bool								parentDebug			READ parentDebug										NOTIFY parentDebugChanged			)
	Q_PROPERTY( bool								focusOnTab			READ focusOnTab				WRITE setFocusOnTab			NOTIFY focusOnTabChanged			)
	Q_PROPERTY( bool								hasError			READ hasError				WRITE setHasError			NOTIFY hasErrorChanged				)
	Q_PROPERTY( bool								hasWarning			READ hasWarning				WRITE setHasWarning			NOTIFY hasWarningChanged			)
	Q_PROPERTY( bool								runOnChange			READ runOnChange			WRITE setRunOnChange		NOTIFY runOnChangeChanged			)
	Q_PROPERTY( bool								initialized			READ initialized										NOTIFY initializedChanged			)
	Q_PROPERTY( bool								shouldShowFocus		READ shouldShowFocus		WRITE setShouldShowFocus	NOTIFY shouldShowFocusChanged		)
	Q_PROPERTY( bool								shouldStealHover	READ shouldStealHover		WRITE setShouldStealHover	NOTIFY shouldStealHoverChanged		)
	Q_PROPERTY( QQuickItem						*	childControlsArea	READ childControlsArea		WRITE setChildControlsArea										)
	Q_PROPERTY( QQuickItem						*	parentListView		READ parentListView										NOTIFY parentListViewChanged		)
	Q_PROPERTY( QQuickItem						*	innerControl		READ innerControl			WRITE setInnerControl		NOTIFY innerControlChanged			)
	Q_PROPERTY( QQuickItem						*	background			READ background				WRITE setBackground			NOTIFY backgroundChanged			)
	Q_PROPERTY( QQuickItem						*	focusIndicator		READ focusIndicator			WRITE setFocusIndicator		NOTIFY focusIndicatorChanged		)
	Q_PROPERTY( QQmlComponent					*	rowComponent		READ rowComponent			WRITE setRowComponent		NOTIFY rowComponentChanged			)
	Q_PROPERTY( QStringList							dependencyMustContain READ dependencyMustContain WRITE setDependencyMustContain NOTIFY dependencyMustContainChanged )
	Q_PROPERTY( int									preferredHeight		READ preferredHeight		WRITE setPreferredHeight	NOTIFY preferredHeightChanged		)
	Q_PROPERTY( int									preferredWidth		READ preferredWidth			WRITE setPreferredWidth		NOTIFY preferredWidthChanged		)
	Q_PROPERTY( int									cursorShape			READ cursorShape			WRITE setCursorShape											)
	Q_PROPERTY( bool								hovered				READ hovered											NOTIFY hoveredChanged				)

public:
	// Any addition here should also be added manually to ControlTypeToFriendlyString... I couldnt get this to work with DECLARE_ENUM...
	enum class ControlType {
		  DefaultControl
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

	JASPControl(QQuickItem *parent = nullptr);

	ControlType		controlType()			const	{ return _controlType;			}
	const QString&	name()					const	{ return _name;					}
	QString			title()					const	{ return _title;				}
	QString			info()					const	{ return _info;					}
	QString			toolTip()				const	{ return _toolTip;				}
	QString			helpMD(int howDeep = 2)	const;
	bool			isBound()				const	{ return _isBound;				}
	bool			indent()				const	{ return _indent;				}
	bool			useControlMouseArea()	const	{ return _useControlMouseArea;	}
	bool			isDependency()			const	{ return _isDependency;			}
	bool			initialized()			const	{ return _initialized;			}
	bool			shouldShowFocus()		const	{ return _shouldShowFocus;		}
	bool			shouldStealHover()		const	{ return _shouldStealHover;		}
	bool			debug()					const	{ return _debug;				}
	bool			parentDebug()			const	{ return _parentDebug;			}
	bool			hasError()				const;
	bool			hasWarning()			const;
	bool			childHasError()			const;
	bool			childHasWarning()		const;
	bool			focusOnTab()			const	{ return activeFocusOnTab();	}
	AnalysisForm*	form()					const	{ return _form;					}
	QQuickItem*		childControlsArea()		const	{ return _childControlsArea;	}
	QQuickItem*		parentListView()		const	{ return _parentListView;		}
	QString			parentListViewKey()		const	{ return _parentListViewKey;	}
	QQuickItem*		innerControl()			const	{ return _innerControl;			}
	QQuickItem*		background()			const	{ return _background;			}
	QQuickItem*		focusIndicator()		const	{ return _focusIndicator;		}
	bool			runOnChange()			const	{ return _runOnChange;			}
	QStringList		dependencyMustContain()	const	{ return _dependencyMustContain; }
	int				preferredHeight()		const	{ return _preferredHeight;		}
	int				preferredWidth()		const	{ return _preferredWidth;		}
	int				cursorShape()			const	{ return _cursorShape;			}
	bool			hovered()				const;

	QString			humanFriendlyLabel()	const;
	void			setInitialized()	{ _initialized = true; emit initializedChanged(); }


	JASPControlWrapper				*	getWrapper()				const { return _wrapper; }
	QQmlComponent					*	rowComponent()				const { return _rowComponent;	}

	static QString					ControlTypeToFriendlyString(ControlType controlType);
	static QList<JASPControl*>		getChildJASPControls(const QQuickItem* item);

public slots:
	void	setControlType(			ControlType			controlType)		{ _controlType = controlType; }
	void	setChildControlsArea(	QQuickItem		*	childControlsArea);
	void	setFocusOnTab(			bool focus);
	void	setHasError(			bool hasError);
	void	setHasWarning(			bool hasWarning);
	void	setRunOnChange(			bool change);
	void	setDebug(				bool debug);
	void	setParentDebug(			bool parentDebug);
	void	setFocusIndicator(		QQuickItem* focusIndicator);
	void	setInnerControl(		QQuickItem* innerControl);
	void	setPreferredHeight(		int preferredHeight, bool isBinding = false);
	void	setPreferredWidth(		int preferredWidth, bool isBinding = false);

	void	addControlError(			QString message);
	void	addControlErrorTemporary(	QString message);
	void	addControlWarning(			QString message);
	void	addControlWarningTemporary(	QString message);
	void	clearControlError();

	void	reconnectWithYourChildren();
	void	listViewKeyChanged(const QString& oldName, const QString& newName);

	GENERIC_SET_FUNCTION(Name				, _name					, nameChanged				, QString		)
	GENERIC_SET_FUNCTION(Info				, _info					, infoChanged				, QString		)
	GENERIC_SET_FUNCTION(ToolTip			, _toolTip				, toolTipChanged			, QString		)
	GENERIC_SET_FUNCTION(Title				, _title				, titleChanged				, QString		)
	GENERIC_SET_FUNCTION(IsBound			, _isBound				, isBoundChanged			, bool			)
	GENERIC_SET_FUNCTION(Indent				, _indent				, indentChanged				, bool			)
	GENERIC_SET_FUNCTION(UseControlMouseArea, _useControlMouseArea	, useControlMouseAreaChanged, bool			)
	GENERIC_SET_FUNCTION(IsDependency		, _isDependency			, isDependencyChanged		, bool			)
	GENERIC_SET_FUNCTION(ShouldShowFocus	, _shouldShowFocus		, shouldShowFocusChanged	, bool			)
	GENERIC_SET_FUNCTION(ShouldStealHover	, _shouldStealHover		, shouldStealHoverChanged	, bool			)
	GENERIC_SET_FUNCTION(Background			, _background			, backgroundChanged			, QQuickItem*	)
	GENERIC_SET_FUNCTION(RowComponent		, _rowComponent			, rowComponentChanged		, QQmlComponent*)
	GENERIC_SET_FUNCTION(DependencyMustContain, _dependencyMustContain, dependencyMustContainChanged, QStringList)

private slots:
	void	_setFocusBorder();
	void	_setShouldShowFocus();
	void	_setBackgroundColor();
	void	_setVisible();
	void	_hoveredChangedSlot() { emit hoveredChanged(); }

signals:
	void setOptionBlockSignal(	bool blockSignal);
	void nameChanged();
	void isBoundChanged();
	void indentChanged();
	void useControlMouseAreaChanged();
	void isDependencyChanged();
	void initializedChanged();
	void shouldShowFocusChanged();
	void shouldStealHoverChanged();
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
	void focusIndicatorChanged();
	void valueChanged();
	void infoChanged();
	void toolTipChanged();
	void titleChanged();
	void helpMDChanged();
	void dependencyMustContainChanged();
	void preferredHeightChanged();
	void preferredWidthChanged();
	void hoveredChanged();

protected:
			void					componentComplete() override;
			void					_setType();
			void					setCursorShape(int shape);

	static	void					appendRowComponent(	QQmlListProperty<QQmlComponent>*, QQmlComponent*);
	static	int						rowComponentsCount(	QQmlListProperty<QQmlComponent>*);
	static	QQmlComponent*			rowComponent(		QQmlListProperty<QQmlComponent>*, int);
	static	void					clearRowComponents(	QQmlListProperty<QQmlComponent>*);


			void					setParentDebugToChildren(bool debug);
			void					setRunOnChangeToChildren(bool change);

protected:
	ControlType				_controlType;
	AnalysisForm*			_form					= nullptr;
	QString					_name,
							_info,
							_toolTip,
							_title,
							_parentListViewKey;
	bool					_isBound				= true,
							_indent					= false,
							_initialized			= false,
							_debug					= false,
							_parentDebug			= false,
							_hasError				= false,
							_hasWarning				= false,
							_isDependency			= false,
							_runOnChange			= true,
							_useControlMouseArea	= true,
							_shouldShowFocus		= false,
							_shouldStealHover		= false;
	JASPControlWrapper	*	_wrapper				= nullptr;
	QQuickItem			*	_parentListView			= nullptr,
						*	_childControlsArea		= nullptr,
						*	_innerControl			= nullptr,
						*	_background				= nullptr,
						*	_focusIndicator			= nullptr;
	QQmlComponent		*	_rowComponent			= nullptr;

	QColor					_defaultBorderColor;
	float					_defaultBorderWidth		= 0;
	QPropertyAnimation		_borderAnimation;
	int						_preferredHeight		= 0,
							_preferredWidth			= 0;
	bool					_preferredHeightBinding	= true,
							_preferredWidthBinding	= true;
	QStringList				_dependencyMustContain;
	QQuickItem			*	_mouseAreaObj			= nullptr;
	int						_cursorShape			= Qt::PointingHandCursor;

	static QMap<QQmlEngine*, QQmlComponent*>		_mouseAreaComponentMap;
	static QByteArray								_mouseAreaDef;
	static QQmlComponent*							getMouseAreaComponent(QQmlEngine* engine);
};


#endif // JASPCONTROL_H
