#ifndef JASPTHEME_H
#define JASPTHEME_H

#include <QQuickItem>
#include <QColor>
#include <QFont>
#include <QFontMetricsF>

#define theme_distanceType	float
#define theme_sizeType		float
#define theme_timeType		int

/// Basic qquickItem we use a theme solution and to easily change theme by simply changing the one referenced in the global environment
/// Currently only two are instantiated by Desktop during loadQml()
class JaspTheme : public QQuickItem
{
	Q_OBJECT

	Q_PROPERTY(float              uiScale                         READ uiScale                                                                  NOTIFY uiScaleChanged                         )
	Q_PROPERTY(float              ribbonScaleHovered              READ ribbonScaleHovered              WRITE setRibbonScaleHovered              NOTIFY ribbonScaleHoveredChanged              )

	//Colors (base):
	Q_PROPERTY(QColor             white                           READ white                           WRITE setWhite                           NOTIFY whiteChanged                           )
	Q_PROPERTY(QColor             whiteBroken                     READ whiteBroken                     WRITE setWhiteBroken                     NOTIFY whiteBrokenChanged                     )
	Q_PROPERTY(QColor             black                           READ black                           WRITE setBlack                           NOTIFY blackChanged                           )
	Q_PROPERTY(QColor             gray                            READ gray                            WRITE setGray                            NOTIFY grayChanged                            )
	Q_PROPERTY(QColor             grayDarker                      READ grayDarker                      WRITE setGrayDarker                      NOTIFY grayDarkerChanged                      )
	Q_PROPERTY(QColor             grayLighter                     READ grayLighter                     WRITE setGrayLighter                     NOTIFY grayLighterChanged                     )
	Q_PROPERTY(QColor             grayMuchLighter                 READ grayMuchLighter                 WRITE setGrayMuchLighter                 NOTIFY grayMuchLighterChanged                 )
	Q_PROPERTY(QColor             grayVeryMuchLighter             READ grayVeryMuchLighter             WRITE setGrayVeryMuchLighter             NOTIFY grayVeryMuchLighterChanged             )
	Q_PROPERTY(QColor             blue                            READ blue                            WRITE setBlue                            NOTIFY blueChanged                            )
	Q_PROPERTY(QColor             blueDarker                      READ blueDarker                      WRITE setBlueDarker                      NOTIFY blueDarkerChanged                      )
	Q_PROPERTY(QColor             blueLighter                     READ blueLighter                     WRITE setBlueLighter                     NOTIFY blueLighterChanged                     )
	Q_PROPERTY(QColor             blueMuchLighter                 READ blueMuchLighter                 WRITE setBlueMuchLighter                 NOTIFY blueMuchLighterChanged                 )
	Q_PROPERTY(QColor             red                             READ red                             WRITE setRed                             NOTIFY redChanged                             )
	Q_PROPERTY(QColor             redDarker                       READ redDarker                       WRITE setRedDarker                       NOTIFY redDarkerChanged                       )
	Q_PROPERTY(QColor             green                           READ green                           WRITE setGreen                           NOTIFY greenChanged                           )
	Q_PROPERTY(QColor             yellowLight                     READ yellowLight                     WRITE setYellowLight                     NOTIFY yellowLightChanged                     )
	Q_PROPERTY(QColor             rose                            READ rose                            WRITE setRose                            NOTIFY roseChanged                            )
	Q_PROPERTY(QColor             roseLight                       READ roseLight                       WRITE setRoseLight                       NOTIFY roseLightChanged                       )
	Q_PROPERTY(QColor             cyan                            READ cyan                            WRITE setCyan                            NOTIFY cyanChanged                            )
	Q_PROPERTY(QColor             shadow                          READ shadow                          WRITE setShadow                          NOTIFY shadowChanged                          )
	Q_PROPERTY(QColor             jaspBlue                        READ jaspBlue                        WRITE setJaspBlue                        NOTIFY jaspBlueChanged                        )
	Q_PROPERTY(QColor             jaspGreen                       READ jaspGreen                       WRITE setJaspGreen                       NOTIFY jaspGreenChanged                       )

	//Colors (ui):
	Q_PROPERTY(QColor             textEnabled                     READ textEnabled                     WRITE setTextEnabled                     NOTIFY textEnabledChanged                     )
	Q_PROPERTY(QColor             textDisabled                    READ textDisabled                    WRITE setTextDisabled                    NOTIFY textDisabledChanged                    )
	Q_PROPERTY(QColor             uiBackground                    READ uiBackground                    WRITE setUiBackground                    NOTIFY uiBackgroundChanged                    )
	Q_PROPERTY(QColor             uiBorder                        READ uiBorder                        WRITE setUiBorder                        NOTIFY uiBorderChanged                        )
	Q_PROPERTY(QColor             fileMenuColorBackground         READ fileMenuColorBackground         WRITE setFileMenuColorBackground         NOTIFY fileMenuColorBackgroundChanged         )
	Q_PROPERTY(QColor             fileMenuLightBorder             READ fileMenuLightBorder             WRITE setFileMenuLightBorder             NOTIFY fileMenuLightBorderChanged             )

	Q_PROPERTY(QColor             buttonColor                     READ buttonColor                     WRITE setButtonColor                     NOTIFY buttonColorChanged                     )
	Q_PROPERTY(QColor             buttonColorHovered              READ buttonColorHovered              WRITE setButtonColorHovered              NOTIFY buttonColorHoveredChanged              )
	Q_PROPERTY(QColor             buttonColorPressed              READ buttonColorPressed              WRITE setButtonColorPressed              NOTIFY buttonColorPressedChanged              )
	Q_PROPERTY(QColor             buttonBorderColor               READ buttonBorderColor               WRITE setButtonBorderColor               NOTIFY buttonBorderColorChanged               )
	Q_PROPERTY(QColor             buttonBorderColorHovered        READ buttonBorderColorHovered        WRITE setButtonBorderColorHovered        NOTIFY buttonBorderColorHoveredChanged        )
	Q_PROPERTY(QColor			  buttonColorDisabled			  MEMBER _buttonColorDisabled													NOTIFY buttonColorDisabledChanged			  )

	Q_PROPERTY(QColor             itemHighlight                   READ itemHighlight                   WRITE setItemHighlight                   NOTIFY itemHighlightChanged                   )
	Q_PROPERTY(QColor             itemHoverColor                  READ itemHoverColor                  WRITE setItemHoverColor                  NOTIFY itemHoverColorChanged                  )
	Q_PROPERTY(QColor             itemSelectedColor               READ itemSelectedColor               WRITE setItemSelectedColor               NOTIFY itemSelectedColorChanged               )
	Q_PROPERTY(QColor             itemSelectedNoFocusColor        READ itemSelectedNoFocusColor        WRITE setItemSelectedNoFocusColor        NOTIFY itemSelectedNoFocusColorChanged        )

	Q_PROPERTY(QColor             darkeningColour                 READ darkeningColour                 WRITE setDarkeningColour                 NOTIFY darkeningColourChanged                 )

	//JASPControl colors mostly:
	Q_PROPERTY(QColor             borderColor                     READ borderColor                     WRITE setBorderColor                     NOTIFY borderColorChanged                     )
	Q_PROPERTY(QColor             focusBorderColor                READ focusBorderColor                WRITE setFocusBorderColor                NOTIFY focusBorderColorChanged                )
	Q_PROPERTY(QColor             dependencyBorderColor           READ dependencyBorderColor           WRITE setDependencyBorderColor           NOTIFY dependencyBorderColorChanged           )
	Q_PROPERTY(QColor             dependencySelectedColor         READ dependencySelectedColor         WRITE setDependencySelectedColor         NOTIFY dependencySelectedColorChanged         )
	Q_PROPERTY(QColor             containsDragBorderColor         READ containsDragBorderColor         WRITE setContainsDragBorderColor         NOTIFY containsDragBorderColorChanged         )

	Q_PROPERTY(QColor             analysisBackgroundColor         READ analysisBackgroundColor         WRITE setAnalysisBackgroundColor         NOTIFY analysisBackgroundColorChanged         )
	Q_PROPERTY(QColor             controlBackgroundColor          READ controlBackgroundColor          WRITE setControlBackgroundColor          NOTIFY controlBackgroundColorChanged          )
	Q_PROPERTY(QColor             controlDisabledBackgroundColor  READ controlDisabledBackgroundColor  WRITE setControlDisabledBackgroundColor  NOTIFY controlDisabledBackgroundColorChanged   )
	Q_PROPERTY(QColor             rowEvenColor                    READ rowEvenColor                    WRITE setRowEvenColor                    NOTIFY rowEvenColorChanged                    )
	Q_PROPERTY(QColor             rowOnevenColor                  READ rowOnevenColor                  WRITE setRowOnevenColor                  NOTIFY rowOnevenColorChanged                  )
	Q_PROPERTY(QColor             controlErrorBackgroundColor     READ controlErrorBackgroundColor     WRITE setControlErrorBackgroundColor     NOTIFY controlErrorBackgroundColorChanged     )
	Q_PROPERTY(QColor             controlErrorTextColor           READ controlErrorTextColor           WRITE setControlErrorTextColor           NOTIFY controlErrorTextColorChanged           )
	Q_PROPERTY(QColor             controlWarningBackgroundColor   READ controlWarningBackgroundColor   WRITE setControlWarningBackgroundColor   NOTIFY controlWarningBackgroundColorChanged   )
	Q_PROPERTY(QColor             controlWarningTextColor         READ controlWarningTextColor         WRITE setControlWarningTextColor         NOTIFY controlWarningTextColorChanged         )

	Q_PROPERTY(QColor             buttonBackgroundColor           READ buttonBackgroundColor           WRITE setButtonBackgroundColor           NOTIFY buttonBackgroundColorChanged           )
	Q_PROPERTY(QColor             tooltipBackgroundColor          READ tooltipBackgroundColor          WRITE setTooltipBackgroundColor          NOTIFY tooltipBackgroundColorChanged          )
	Q_PROPERTY(QColor             debugBackgroundColor            READ debugBackgroundColor            WRITE setDebugBackgroundColor            NOTIFY debugBackgroundColorChanged            )
	Q_PROPERTY(QColor             errorMessagesBackgroundColor    READ errorMessagesBackgroundColor    WRITE setErrorMessagesBackgroundColor    NOTIFY errorMessagesBackgroundColorChanged    )
	Q_PROPERTY(QColor             sliderPartOn                    READ sliderPartOn                    WRITE setSliderPartOn                    NOTIFY sliderPartOnChanged                    )
	Q_PROPERTY(QColor             sliderPartOff                   READ sliderPartOff                   WRITE setSliderPartOff                   NOTIFY sliderPartOffChanged                   )

	//Distances:
	Q_PROPERTY(theme_distanceType borderRadius                    READ borderRadius                    WRITE setBorderRadius                    NOTIFY borderRadiusChanged                    )
	Q_PROPERTY(theme_distanceType shadowRadius                    READ shadowRadius                    WRITE setShadowRadius                    NOTIFY shadowRadiusChanged                    )

	Q_PROPERTY(theme_distanceType itemPadding                     READ itemPadding                     WRITE setItemPadding                     NOTIFY itemPaddingChanged                     )
	Q_PROPERTY(theme_distanceType jaspControlPadding              READ jaspControlPadding              WRITE setJaspControlPadding              NOTIFY jaspControlPaddingChanged              )
	Q_PROPERTY(theme_distanceType ribbonButtonPadding             READ ribbonButtonPadding             WRITE setRibbonButtonPadding             NOTIFY ribbonButtonPaddingChanged             )
	Q_PROPERTY(theme_distanceType groupContentPadding             READ groupContentPadding             WRITE setGroupContentPadding             NOTIFY groupContentPaddingChanged             )

	Q_PROPERTY(theme_distanceType rowSpacing                      READ rowSpacing                      WRITE setRowSpacing                      NOTIFY rowSpacingChanged                      )
	Q_PROPERTY(theme_distanceType rowGridSpacing                  READ rowGridSpacing                  WRITE setRowGridSpacing                  NOTIFY rowGridSpacingChanged                  )
	Q_PROPERTY(theme_distanceType rowGroupSpacing                 READ rowGroupSpacing                 WRITE setRowGroupSpacing                 NOTIFY rowGroupSpacingChanged                 )
	Q_PROPERTY(theme_distanceType columnGridSpacing               READ columnGridSpacing               WRITE setColumnGridSpacing               NOTIFY columnGridSpacingChanged               )
	Q_PROPERTY(theme_distanceType columnGroupSpacing              READ columnGroupSpacing              WRITE setColumnGroupSpacing              NOTIFY columnGroupSpacingChanged              )
	Q_PROPERTY(theme_distanceType indentationLength               READ indentationLength               WRITE setIndentationLength               NOTIFY indentationLengthChanged               )
	Q_PROPERTY(theme_distanceType labelSpacing                    READ labelSpacing                    WRITE setLabelSpacing                    NOTIFY labelSpacingChanged                    )
	Q_PROPERTY(theme_distanceType menuSpacing                     READ menuSpacing                     WRITE setMenuSpacing                     NOTIFY menuSpacingChanged                     )
	Q_PROPERTY(theme_distanceType menuPadding                     READ menuPadding                     WRITE setMenuPadding                     NOTIFY menuPaddingChanged                     )

	Q_PROPERTY(theme_distanceType generalAnchorMargin             READ generalAnchorMargin             WRITE setGeneralAnchorMargin             NOTIFY generalAnchorMarginChanged             )
	Q_PROPERTY(theme_distanceType generalMenuMargin               READ generalMenuMargin               WRITE setGeneralMenuMargin               NOTIFY generalMenuMarginChanged               )
	Q_PROPERTY(theme_distanceType titleBottomMargin               READ titleBottomMargin               WRITE setTitleBottomMargin               NOTIFY titleBottomMarginChanged               )
	Q_PROPERTY(theme_distanceType contentMargin		              READ contentMargin				   WRITE setContentMargin		            NOTIFY contentMarginChanged		              )
	Q_PROPERTY(theme_distanceType subOptionOffset                 READ subOptionOffset                 WRITE setSubOptionOffset                 NOTIFY subOptionOffsetChanged                 )

	//Sizes:
	Q_PROPERTY(theme_sizeType     minPanelWidth                   READ minPanelWidth                   WRITE setMinPanelWidth                   NOTIFY minPanelWidthChanged                   )
	Q_PROPERTY(theme_sizeType     resultWidth                     READ resultWidth                     WRITE setResultWidth                     NOTIFY resultWidthChanged                     )
	Q_PROPERTY(theme_sizeType     formWidth                       READ formWidth                       WRITE setFormWidth                       NOTIFY formWidthChanged                       )
	Q_PROPERTY(theme_sizeType     iconSize                        READ iconSize                        WRITE setIconSize                        NOTIFY iconSizeChanged                        )
	Q_PROPERTY(theme_sizeType     formMargin                      READ formMargin                      WRITE setFormMargin                      NOTIFY formMarginChanged                      )
	Q_PROPERTY(theme_sizeType     sliderWidth                     READ sliderWidth                     WRITE setSliderWidth                     NOTIFY sliderWidthChanged                     )
	Q_PROPERTY(theme_sizeType     sliderLength                    READ sliderLength                    WRITE setSliderLength                    NOTIFY sliderLengthChanged                    )
	Q_PROPERTY(theme_sizeType     switchHeight                    READ switchHeight                    WRITE setSwitchHeight                    NOTIFY switchHeightChanged                    )
	Q_PROPERTY(theme_sizeType     spinBoxHeight                   READ spinBoxHeight                   WRITE setSpinBoxHeight                   NOTIFY spinBoxHeightChanged                   )
	Q_PROPERTY(theme_sizeType     spinBoxWidth                    READ spinBoxWidth                    WRITE setSpinBoxWidth                    NOTIFY spinBoxWidthChanged                    )
	Q_PROPERTY(theme_sizeType     comboBoxHeight                  READ comboBoxHeight                  WRITE setComboBoxHeight                  NOTIFY comboBoxHeightChanged                  )
	Q_PROPERTY(theme_sizeType     textFieldWidth                  READ textFieldWidth                  WRITE setTextFieldWidth                  NOTIFY textFieldWidthChanged                  )
	Q_PROPERTY(theme_sizeType     textFieldHeight                 READ textFieldHeight                 WRITE setTextFieldHeight                 NOTIFY textFieldHeightChanged                 )
	Q_PROPERTY(theme_sizeType     numericFieldWidth               READ numericFieldWidth               WRITE setNumericFieldWidth               NOTIFY numericFieldWidthChanged               )
	Q_PROPERTY(theme_sizeType     splitHandleWidth                READ splitHandleWidth                WRITE setSplitHandleWidth                NOTIFY splitHandleWidthChanged                )
	Q_PROPERTY(theme_sizeType     subMenuIconHeight               READ subMenuIconHeight               WRITE setSubMenuIconHeight               NOTIFY subMenuIconHeightChanged               )
	Q_PROPERTY(theme_sizeType     ribbonButtonHeight              READ ribbonButtonHeight              WRITE setRibbonButtonHeight              NOTIFY ribbonButtonHeightChanged              )
	Q_PROPERTY(theme_sizeType     variablesListTitle              READ variablesListTitle              WRITE setVariablesListTitle              NOTIFY variablesListTitleChanged              )
	Q_PROPERTY(theme_sizeType     sliderHandleDiameter            READ sliderHandleDiameter            WRITE setSliderHandleDiameter            NOTIFY sliderHandleDiameterChanged            )
	Q_PROPERTY(theme_sizeType     defaultTextAreaHeight           READ defaultTextAreaHeight           WRITE setDefaultTextAreaHeight           NOTIFY defaultTextAreaHeightChanged           )
	Q_PROPERTY(theme_sizeType     formExpanderHeaderHeight        READ formExpanderHeaderHeight        WRITE setFormExpanderHeaderHeight        NOTIFY formExpanderHeaderHeightChanged        )
	Q_PROPERTY(theme_sizeType     jaspControlHighlightWidth       READ jaspControlHighlightWidth       WRITE setJaspControlHighlightWidth       NOTIFY jaspControlHighlightWidthChanged       )
	Q_PROPERTY(theme_sizeType     defaultVariablesFormHeight      READ defaultVariablesFormHeight      WRITE setDefaultVariablesFormHeight      NOTIFY defaultVariablesFormHeightChanged      )
	Q_PROPERTY(theme_sizeType     defaultSingleItemListHeight     READ defaultSingleItemListHeight     WRITE setDefaultSingleItemListHeight     NOTIFY defaultSingleItemListHeightChanged     )
	Q_PROPERTY(theme_sizeType     defaultRectangularButtonHeight  READ defaultRectangularButtonHeight  WRITE setDefaultRectangularButtonHeight  NOTIFY defaultRectangularButtonHeightChanged  )
	Q_PROPERTY(theme_sizeType     smallDefaultVariablesFormHeight READ smallDefaultVariablesFormHeight WRITE setSmallDefaultVariablesFormHeight NOTIFY smallDefaultVariablesFormHeightChanged )
	Q_PROPERTY(theme_sizeType     messageBoxButtonHeight          READ messageBoxButtonHeight          WRITE setMessageBoxButtonHeight          NOTIFY messageBoxButtonHeightChanged          )
	Q_PROPERTY(theme_sizeType     scrollbarBoxWidthBig            READ scrollbarBoxWidthBig            WRITE setScrollbarBoxWidthBig            NOTIFY scrollbarBoxWidthBigChanged            )
	Q_PROPERTY(theme_sizeType     scrollbarBoxWidth               READ scrollbarBoxWidth               WRITE setScrollbarBoxWidth               NOTIFY scrollbarBoxWidthChanged               )
	Q_PROPERTY(theme_sizeType     menuItemHeight                  READ menuItemHeight                  WRITE setMenuItemHeight                  NOTIFY menuItemHeightChanged                  )
	Q_PROPERTY(theme_sizeType     menuGroupTitleHeight            READ menuGroupTitleHeight            WRITE setMenuGroupTitleHeight            NOTIFY menuGroupTitleHeightChanged            )
	Q_PROPERTY(theme_sizeType     menuHeaderHeight                READ menuHeaderHeight                WRITE setMenuHeaderHeight                NOTIFY menuHeaderHeightChanged                )

	//Velocities:
	Q_PROPERTY(float              maximumFlickVelocity            READ maximumFlickVelocity                                                     NOTIFY maximumFlickVelocityChanged            )

	//Times: https://www.youtube.com/watch?v=90WD_ats6eE
	//typedef int int;
	Q_PROPERTY(theme_timeType     hoverTime                       READ hoverTime                       WRITE setHoverTime                       NOTIFY hoverTimeChanged                       )
	Q_PROPERTY(theme_timeType     fileMenuSlideDuration           READ fileMenuSlideDuration           WRITE setFileMenuSlideDuration           NOTIFY fileMenuSlideDurationChanged           )
	Q_PROPERTY(theme_timeType     toolTipDelay                    READ toolTipDelay                    WRITE setToolTipDelay                    NOTIFY toolTipDelayChanged                    )
	Q_PROPERTY(theme_timeType     toolTipTimeout                  READ toolTipTimeout                  WRITE setToolTipTimeout                  NOTIFY toolTipTimeoutChanged                  )

	Q_PROPERTY(QFont              font                            READ font                            WRITE setFont                            NOTIFY fontChanged                            )
	Q_PROPERTY(QFont              fontLabel                       READ fontLabel                       WRITE setFontLabel                       NOTIFY fontLabelChanged                       )
	Q_PROPERTY(QFont              fontRibbon                      READ fontRibbon                      WRITE setFontRibbon                      NOTIFY fontRibbonChanged                      )
	Q_PROPERTY(QFont              fontGroupTitle                  READ fontGroupTitle                  WRITE setFontGroupTitle                  NOTIFY fontGroupTitleChanged                  )
	Q_PROPERTY(QFont              fontPrefOptionsGroupTitle       READ fontPrefOptionsGroupTitle       WRITE setFontPrefOptionsGroupTitle       NOTIFY fontPrefOptionsGroupTitleChanged       )

	Q_PROPERTY(QFont              fontRCode                       READ fontRCode                       WRITE setFontRCode                       NOTIFY fontRCodeChanged                       )
	Q_PROPERTY(QFont              fontCode						  READ fontCode				           WRITE setFontCode                        NOTIFY fontCodeChanged                        )

	//Iconfolder:
	Q_PROPERTY(QString            iconPath                        READ iconPath                                                                 NOTIFY iconPathChanged                        )
	Q_PROPERTY(QString            themeName                       READ themeName                       WRITE setThemeName                       NOTIFY themeNameChanged                       )

	Q_PROPERTY(bool               isDark                          READ isDark                          WRITE setIsDark                          NOTIFY isDarkChanged						  )

public:
	explicit JaspTheme(QQuickItem * item = nullptr);
	~JaspTheme();

	static void setCurrentTheme(JaspTheme * theme);
	static void setCurrentThemeFromName(QString name);

	static JaspTheme								* currentTheme()	{ return _currentTheme; }
	static QFontMetricsF							& fontMetrics()		{ return _fontMetrics;  } //For qml interface font used everywhere (in particular in datasetview though)
	static const std::map<QString, JaspTheme *>		& themes()			{ return _themes;		}

	float				uiScale()							const	{ return _uiScale;				}
	float				ribbonScaleHovered()				const	{ return _ribbonScaleHovered;	}
	QColor				white()								const	{ return _white; }
	QColor				whiteBroken()						const	{ return _whiteBroken; }
	QColor				black()								const	{ return _black; }
	QColor				gray()								const	{ return _gray; }
	QColor				grayDarker()						const	{ return _grayDarker; }
	QColor				grayLighter()						const	{ return _grayLighter; }
	QColor				grayMuchLighter()					const	{ return _grayMuchLighter; }
	QColor				grayVeryMuchLighter()				const	{ return _grayVeryMuchLighter; }
	QColor				blue()								const	{ return _blue; }
	QColor				blueDarker()						const	{ return _blueDarker; }
	QColor				blueLighter()						const	{ return _blueLighter; }
	QColor				blueMuchLighter()					const	{ return _blueMuchLighter; }
	QColor				red()								const	{ return _red; }
	QColor				redDarker()							const	{ return _redDarker; }
	QColor				green()								const	{ return _green; }
	QColor				yellowLight()						const	{ return _yellowLight; }
	QColor				rose()								const	{ return _rose; }
	QColor				roseLight()							const	{ return _roseLight; }
	QColor				cyan()								const	{ return _cyan; }
	QColor				shadow()							const	{ return _shadow; }
	QColor				jaspBlue()							const	{ return _jaspBlue;	}
	QColor				jaspGreen()							const	{ return _jaspGreen; }
	QColor				textEnabled()						const	{ return _textEnabled; }
	QColor				textDisabled()						const	{ return _textDisabled; }
	QColor				uiBackground()						const	{ return _uiBackground; }
	QColor				uiBorder()							const	{ return _uiBorder; }
	QColor				fileMenuColorBackground()			const	{ return _fileMenuColorBackground; }
	QColor				fileMenuLightBorder()				const	{ return _fileMenuLightBorder; }
	QColor				buttonColor()						const	{ return _buttonColor; }
	QColor				buttonColorHovered()				const	{ return _buttonColorHovered; }
	QColor				buttonColorPressed()				const	{ return _buttonColorPressed; }
	QColor				buttonBorderColor()					const	{ return _buttonBorderColor; }
	QColor				buttonBorderColorHovered()			const	{ return _buttonBorderColorHovered; }
	QColor				itemHighlight()						const	{ return _itemHighlight; }
	QColor				itemHoverColor()					const	{ return _itemHoverColor; }
	QColor				itemSelectedColor()					const	{ return _itemSelectedColor; }
	QColor				itemSelectedNoFocusColor()			const	{ return _itemSelectedNoFocusColor; }
	QColor				borderColor()						const	{ return _borderColor; }
	QColor				focusBorderColor()					const	{ return _focusBorderColor; }
	QColor				dependencyBorderColor()				const	{ return _dependencyBorderColor; }
	QColor				dependencySelectedColor()			const	{ return _dependencySelectedColor; }
	QColor				containsDragBorderColor()			const	{ return _containsDragBorderColor; }
	QColor				analysisBackgroundColor()			const	{ return _analysisBackgroundColor; }
	QColor				controlBackgroundColor()			const	{ return _controlBackgroundColor; }
	QColor				controlDisabledBackgroundColor()	const	{ return _controlDisabledBackgroundColor; }
	QColor				rowEvenColor()						const	{ return _rowEvenColor; }
	QColor				rowOnevenColor() 					const	{ return _rowOnevenColor; }
	QColor				controlErrorBackgroundColor() 		const	{ return _controlErrorBackgroundColor; }
	QColor				controlErrorTextColor() 			const	{ return _controlErrorTextColor; }
	QColor				controlWarningBackgroundColor() 	const	{ return _controlWarningBackgroundColor; }
	QColor				controlWarningTextColor() 			const	{ return _controlWarningTextColor; }
	QColor				buttonBackgroundColor() 			const	{ return _buttonBackgroundColor; }
	QColor				tooltipBackgroundColor() 			const	{ return _tooltipBackgroundColor; }
	QColor				debugBackgroundColor() 				const	{ return _debugBackgroundColor; }
	QColor				errorMessagesBackgroundColor() 		const	{ return _errorMessagesBackgroundColor; }
	QColor				sliderPartOn() 						const	{ return _sliderPartOn; }
	QColor				sliderPartOff() 					const	{ return _sliderPartOff; }
	QColor				darkeningColour()					const	{ return _darkeningColour;	}
	theme_distanceType	borderRadius()						const	{ return _borderRadius						* uiScale(); }
	theme_distanceType	shadowRadius()						const	{ return _shadowRadius						* uiScale(); }
	theme_distanceType	itemPadding()						const	{ return _itemPadding						* uiScale(); }
	theme_distanceType	jaspControlPadding()				const	{ return _jaspControlPadding				* uiScale(); }
	theme_distanceType	ribbonButtonPadding()				const	{ return _ribbonButtonPadding				* uiScale(); }
	theme_distanceType	groupContentPadding()				const	{ return _groupContentPadding				* uiScale(); }
	theme_distanceType	rowSpacing()						const	{ return _rowSpacing						* uiScale(); }
	theme_distanceType	rowGridSpacing()					const	{ return _rowGridSpacing					* uiScale(); }
	theme_distanceType	rowGroupSpacing()					const	{ return _rowGroupSpacing					* uiScale(); }
	theme_distanceType	columnGridSpacing()					const	{ return _columnGridSpacing					* uiScale(); }
	theme_distanceType	columnGroupSpacing()				const	{ return _columnGroupSpacing				* uiScale(); }
	theme_distanceType	indentationLength()					const	{ return _indentationLength					* uiScale(); }
	theme_distanceType	labelSpacing()						const	{ return _labelSpacing						* uiScale(); }
	theme_distanceType	menuSpacing()						const	{ return _menuSpacing						* uiScale(); }
	theme_distanceType	menuPadding()						const	{ return _menuPadding						* uiScale(); }
	theme_distanceType	generalAnchorMargin()				const	{ return _generalAnchorMargin				* uiScale(); }
	theme_distanceType	generalMenuMargin()					const	{ return _generalMenuMargin					* uiScale(); }
	theme_distanceType	titleBottomMargin()					const	{ return _titleBottomMargin					* uiScale(); }
	theme_distanceType	contentMargin()						const	{ return _contentMargin						* uiScale(); }
	theme_distanceType	subOptionOffset()					const	{ return _subOptionOffset					* uiScale(); }
	theme_sizeType		minPanelWidth()						const	{ return _minPanelWidth						* uiScale(); }
	theme_sizeType		resultWidth()						const	{ return _resultWidth						* uiScale(); }
	theme_sizeType		formWidth()							const	{ return _formWidth							* uiScale(); }
	theme_sizeType		iconSize()							const	{ return _iconSize							* uiScale(); }
	theme_sizeType		formMargin()						const	{ return _formMargin						* uiScale(); }
	theme_sizeType		formExpanderHeaderHeight()			const	{ return _formExpanderHeaderHeight			* uiScale(); }
	theme_sizeType		sliderWidth()						const	{ return _sliderWidth						* uiScale(); }
	theme_sizeType		sliderLength()						const	{ return _sliderLength						* uiScale(); }
	theme_sizeType		switchHeight()						const	{ return _switchHeight						* uiScale(); }
	theme_sizeType		spinBoxHeight()						const	{ return _spinBoxHeight						* uiScale(); }
	theme_sizeType		spinBoxWidth()						const	{ return _spinBoxWidth						* uiScale(); }
	theme_sizeType		comboBoxHeight()					const	{ return _comboBoxHeight					* uiScale(); }
	theme_sizeType		textFieldWidth()					const	{ return _textFieldWidth					* uiScale(); }
	theme_sizeType		textFieldHeight()					const	{ return _textFieldHeight					* uiScale(); }
	theme_sizeType		numericFieldWidth()					const	{ return _numericFieldWidth					* uiScale(); }
	theme_sizeType		splitHandleWidth()					const	{ return _splitHandleWidth					* uiScale(); }
	theme_sizeType		subMenuIconHeight()					const	{ return _subMenuIconHeight					* uiScale(); }
	theme_sizeType		ribbonButtonHeight()				const	{ return _ribbonButtonHeight				* uiScale(); }
	theme_sizeType		variablesListTitle()				const	{ return _variablesListTitle				* uiScale(); }
	theme_sizeType		sliderHandleDiameter()				const	{ return _sliderHandleDiameter				* uiScale(); }
	theme_sizeType		defaultTextAreaHeight()				const	{ return _defaultTextAreaHeight				* uiScale(); }
	theme_sizeType		jaspControlHighlightWidth()			const	{ return _jaspControlHighlightWidth			* uiScale(); }
	theme_sizeType		defaultVariablesFormHeight()		const	{ return _defaultVariablesFormHeight		* uiScale(); }
	theme_sizeType		defaultSingleItemListHeight()		const	{ return _defaultSingleItemListHeight		* uiScale(); }
	theme_sizeType		defaultRectangularButtonHeight()	const	{ return _defaultRectangularButtonHeight	* uiScale(); }
	theme_sizeType		smallDefaultVariablesFormHeight()	const	{ return _smallDefaultVariablesFormHeight	* uiScale(); }
	theme_sizeType		messageBoxButtonHeight()			const	{ return _messageBoxButtonHeight			* uiScale(); }
	theme_sizeType		scrollbarBoxWidthBig()				const	{ return _scrollbarBoxWidthBig				* uiScale(); }
	theme_sizeType		scrollbarBoxWidth()					const	{ return _scrollbarBoxWidth					* uiScale(); }
	theme_sizeType		menuItemHeight()					const	{ return _menuItemHeight					* uiScale(); }
	theme_sizeType		menuGroupTitleHeight()				const	{ return _menuGroupTitleHeight				* uiScale(); }
	theme_sizeType		menuHeaderHeight()					const	{ return _menuHeaderHeight					* uiScale(); }
	float				maximumFlickVelocity()				const	{ return _maximumFlickVelocity;				}
	int					hoverTime()							const	{ return _hoverTime;					}
	int					fileMenuSlideDuration()				const	{ return _fileMenuSlideDuration;		}
	int					toolTipDelay()						const	{ return _toolTipDelay;					}
	int					toolTipTimeout()					const	{ return _toolTipTimeout;				}
	QFont				font()								const	{ return _font;							}
	QFont				fontLabel()							const	{ return _fontLabel;					}
	QFont				fontRibbon()						const	{ return _fontRibbon;					}
	QFont				fontGroupTitle()					const	{ return _fontGroupTitle;				}
	QFont				fontPrefOptionsGroupTitle()			const	{ return _fontPrefOptionsGroupTitle;	}
	QFont				fontRCode()							const	{ return _fontRCode;					}
	QFont				fontCode()							const	{ return _fontCode;						}
	QString				iconPath()							const	{ return _iconPath;						}
	QString				themeName()							const	{ return _themeName;					}
	static QString		currentIconPath();
	bool				isDark()							const	{ return _isDark;						}

signals:
	void currentThemeReady(JaspTheme * newTheme);
	void uiScaleChanged(float uiScale);
	void ribbonScaleHoveredChanged(float ribbonScaleHovered);
	void whiteChanged(QColor white);
	void whiteBrokenChanged(QColor whiteBroken);
	void blackChanged(QColor black);
	void grayChanged(QColor gray);
	void grayDarkerChanged(QColor grayDarker);
	void grayLighterChanged(QColor grayLighter);
	void grayMuchLighterChanged(QColor grayMuchLighter);
	void grayVeryMuchLighterChanged(QColor grayVeryMuchLighter);
	void blueChanged(QColor blue);
	void blueDarkerChanged(QColor blueDarker);
	void blueLighterChanged(QColor blueLighter);
	void blueMuchLighterChanged(QColor blueMuchLighter);
	void redChanged(QColor red);
	void redDarkerChanged(QColor redDarker);
	void greenChanged(QColor green);
	void yellowLightChanged(QColor yellowLight);
	void roseChanged(QColor rose);
	void roseLightChanged(QColor roseLight);
	void cyanChanged(QColor cyan);
	void shadowChanged(QColor shadow);
	void jaspBlueChanged(QColor jaspBlue);
	void jaspGreenChanged(QColor jaspGreen);
	void textEnabledChanged(QColor textEnabled);
	void textDisabledChanged(QColor textDisabled);
	void uiBackgroundChanged(QColor uiBackground);
	void uiBorderChanged(QColor uiBorder);
	void fileMenuColorBackgroundChanged(QColor fileMenuColorBackground);
	void fileMenuLightBorderChanged(QColor fileMenuLightBorder);
	void buttonColorChanged(QColor buttonColor);
	void buttonColorHoveredChanged(QColor buttonColorHovered);
	void buttonColorPressedChanged(QColor buttonColorPressed);
	void buttonColorDisabledChanged(QColor buttonColorPressed);
	void buttonBorderColorChanged(QColor buttonBorderColor);
	void buttonBorderColorHoveredChanged(QColor buttonBorderColorHovered);
	void itemHighlightChanged(QColor itemHighlight);
	void itemHoverColorChanged(QColor itemHoverColor);
	void itemSelectedColorChanged(QColor itemSelectedColor);
	void itemSelectedNoFocusColorChanged(QColor itemSelectedNoFocusColor);
	void borderColorChanged(QColor borderColor);
	void focusBorderColorChanged(QColor focusBorderColor);
	void dependencyBorderColorChanged(QColor dependencyBorderColor);
	void dependencySelectedColorChanged(QColor dependencySelectedColor);
	void containsDragBorderColorChanged(QColor containsDragBorderColor);
	void analysisBackgroundColorChanged(QColor analysisBackgroundColor);
	void controlBackgroundColorChanged(QColor controlBackgroundColor);
	void controlDisabledBackgroundColorChanged(QColor controlDisabledBackgroundColor);
	void rowEvenColorChanged(QColor rowEvenColor);
	void rowOnevenColorChanged(QColor rowOnevenColor);
	void controlErrorBackgroundColorChanged(QColor controlErrorBackgroundColor);
	void controlErrorTextColorChanged(QColor controlErrorTextColor);
	void controlWarningBackgroundColorChanged(QColor controlWarningBackgroundColor);
	void controlWarningTextColorChanged(QColor controlWarningTextColor);
	void buttonBackgroundColorChanged(QColor buttonBackgroundColor);
	void tooltipBackgroundColorChanged(QColor tooltipBackgroundColor);
	void debugBackgroundColorChanged(QColor debugBackgroundColor);
	void errorMessagesBackgroundColorChanged(QColor errorMessagesBackgroundColor);
	void sliderPartOnChanged(QColor sliderPartOn);
	void sliderPartOffChanged(QColor sliderPartOff);
	void darkeningColourChanged(QColor darkeningColour);
	void borderRadiusChanged();
	void shadowRadiusChanged();
	void itemPaddingChanged();
	void jaspControlPaddingChanged();
	void ribbonButtonPaddingChanged();
	void groupContentPaddingChanged();
	void rowSpacingChanged();
	void rowGridSpacingChanged();
	void rowGroupSpacingChanged();
	void columnGridSpacingChanged();
	void columnGroupSpacingChanged();
	void indentationLengthChanged();
	void labelSpacingChanged();
	void menuSpacingChanged();
	void menuPaddingChanged();
	void generalAnchorMarginChanged();
	void generalMenuMarginChanged();
	void titleBottomMarginChanged();
	void contentMarginChanged();
	void subOptionOffsetChanged();
	void minPanelWidthChanged();
	void resultWidthChanged();
	void formWidthChanged();
	void iconSizeChanged();
	void formMarginChanged();
	void formExpanderHeaderHeightChanged();
	void sliderWidthChanged();
	void sliderLengthChanged();
	void switchHeightChanged();
	void spinBoxHeightChanged();
	void spinBoxWidthChanged();
	void comboBoxHeightChanged();
	void textFieldWidthChanged();
	void textFieldHeightChanged();
	void numericFieldWidthChanged();
	void splitHandleWidthChanged();
	void subMenuIconHeightChanged();
	void ribbonButtonHeightChanged();
	void variablesListTitleChanged();
	void sliderHandleDiameterChanged();
	void defaultTextAreaHeightChanged();
	void jaspControlHighlightWidthChanged();
	void defaultVariablesFormHeightChanged();
	void defaultSingleItemListHeightChanged();
	void defaultRectangularButtonHeightChanged();
	void smallDefaultVariablesFormHeightChanged();
	void messageBoxButtonHeightChanged();
	void scrollbarBoxWidthBigChanged();
	void scrollbarBoxWidthChanged();
	void menuItemHeightChanged();
	void menuGroupTitleHeightChanged();
	void menuHeaderHeightChanged();
	void maximumFlickVelocityChanged();
	void hoverTimeChanged(theme_timeType hoverTime);
	void fileMenuSlideDurationChanged(theme_timeType fileMenuSlideDuration);
	void toolTipDelayChanged(theme_timeType toolTipDelay);
	void toolTipTimeoutChanged(theme_timeType toolTipTimeout);
	void fontChanged(QFont font);
	void fontLabelChanged(QFont fontLabel);
	void fontRibbonChanged(QFont fontRibbon);
	void fontGroupTitleChanged(QFont fontGroupTitle);
	void fontPrefOptionsGroupTitleChanged(QFont fontPrefOptionsGroupTitle);
	void fontRCodeChanged(QFont fontRCode);
	void fontCodeChanged(QFont fontCode);
	void iconPathChanged(QString iconPath);
	void themeNameChanged(QString themeName);
	void currentThemeNameChanged();
	void isDarkChanged(bool isDark);

public slots:
	void setRibbonScaleHovered(float ribbonScaleHovered);
	void setWhite(QColor white);
	void setWhiteBroken(QColor whiteBroken);
	void setBlack(QColor black);
	void setGray(QColor gray);
	void setGrayDarker(QColor grayDarker);
	void setGrayLighter(QColor grayLighter);
	void setGrayMuchLighter(QColor grayMuchLighter);
	void setGrayVeryMuchLighter(QColor grayVeryMuchLighter);
	void setBlue(QColor blue);
	void setBlueDarker(QColor blueDarker);
	void setBlueLighter(QColor blueLighter);
	void setBlueMuchLighter(QColor blueMuchLighter);
	void setRed(QColor red);
	void setRedDarker(QColor redDarker);
	void setGreen(QColor green);
	void setYellowLight(QColor yellowLight);
	void setRose(QColor rose);
	void setRoseLight(QColor roseLight);
	void setCyan(QColor cyan);
	void setShadow(QColor shadow);
	void setJaspBlue(QColor jaspBlue);
	void setJaspGreen(QColor jaspGreen);
	void setTextEnabled(QColor textEnabled);
	void setTextDisabled(QColor textDisabled);
	void setUiBackground(QColor uiBackground);
	void setUiBorder(QColor uiBorder);
	void setFileMenuColorBackground(QColor fileMenuColorBackground);
	void setFileMenuLightBorder(QColor fileMenuLightBorder);
	void setButtonColor(QColor buttonColor);
	void setButtonColorHovered(QColor buttonColorHovered);
	void setButtonColorPressed(QColor buttonColorPressed);
	void setButtonBorderColor(QColor buttonBorderColor);
	void setButtonBorderColorHovered(QColor buttonBorderColorHovered);
	void setItemHighlight(QColor itemHighlight);
	void setItemHoverColor(QColor itemHoverColor);
	void setItemSelectedColor(QColor itemSelectedColor);
	void setItemSelectedNoFocusColor(QColor itemSelectedNoFocusColor);
	void setBorderColor(QColor borderColor);
	void setFocusBorderColor(QColor focusBorderColor);
	void setDependencyBorderColor(QColor dependencyBorderColor);
	void setDependencySelectedColor(QColor dependencySelectedColor);
	void setContainsDragBorderColor(QColor containsDragBorderColor);
	void setAnalysisBackgroundColor(QColor analysisBackgroundColor);
	void setControlBackgroundColor(QColor controlBackgroundColor);
	void setControlDisabledBackgroundColor(QColor controlDisabledBackgroundColor);
	void setRowEvenColor(QColor rowEvenColor);
	void setRowOnevenColor(QColor rowOnevenColor);
	void setControlErrorBackgroundColor(QColor controlErrorBackgroundColor);
	void setControlErrorTextColor(QColor controlErrorTextColor);
	void setControlWarningBackgroundColor(QColor controlWarningBackgroundColor);
	void setControlWarningTextColor(QColor controlWarningTextColor);
	void setButtonBackgroundColor(QColor buttonBackgroundColor);
	void setTooltipBackgroundColor(QColor tooltipBackgroundColor);
	void setDebugBackgroundColor(QColor debugBackgroundColor);
	void setErrorMessagesBackgroundColor(QColor errorMessagesBackgroundColor);
	void setSliderPartOn(QColor sliderPartOn);
	void setSliderPartOff(QColor sliderPartOff);
	void setDarkeningColour(QColor darkeningColour);
	void setBorderRadius(theme_distanceType borderRadius);
	void setShadowRadius(theme_distanceType shadowRadius);
	void setItemPadding(theme_distanceType itemPadding);
	void setJaspControlPadding(theme_distanceType jaspControlPadding);
	void setRibbonButtonPadding(theme_distanceType ribbonButtonPadding);
	void setGroupContentPadding(theme_distanceType groupContentPadding);
	void setRowSpacing(theme_distanceType rowSpacing);
	void setRowGridSpacing(theme_distanceType rowGridSpacing);
	void setRowGroupSpacing(theme_distanceType rowGroupSpacing);
	void setColumnGridSpacing(theme_distanceType columnGridSpacing);
	void setColumnGroupSpacing(theme_distanceType columnGroupSpacing);
	void setIndentationLength(theme_distanceType indentationLength);
	void setLabelSpacing(theme_distanceType labelSpacing);
	void setMenuSpacing(theme_distanceType menuSpacing);
	void setMenuPadding(theme_distanceType menuPadding);
	void setGeneralAnchorMargin(theme_distanceType generalAnchorMargin);
	void setGeneralMenuMargin(theme_distanceType generalMenuMargin);
	void setTitleBottomMargin(theme_distanceType titleBottomMargin);
	void setContentMargin(theme_distanceType contentMargin);
	void setSubOptionOffset(theme_distanceType subOptionOffset);
	void setMinPanelWidth(theme_sizeType minPanelWidth);
	void setResultWidth(theme_sizeType resultWidth);
	void setFormWidth(theme_sizeType formWidth);
	void setIconSize(theme_sizeType iconSize);
	void setFormMargin(theme_sizeType formMargin);
	void setFormExpanderHeaderHeight(theme_sizeType formExpanderHeaderHeight);
	void setSliderWidth(theme_sizeType sliderWidth);
	void setSliderLength(theme_sizeType sliderLength);
	void setSwitchHeight(theme_sizeType switchHeight);
	void setSpinBoxHeight(theme_sizeType spinBoxHeight);
	void setSpinBoxWidth(theme_sizeType spinBoxWidth);
	void setComboBoxHeight(theme_sizeType comboBoxHeight);
	void setTextFieldWidth(theme_sizeType textFieldWidth);
	void setTextFieldHeight(theme_sizeType textFieldHeight);
	void setNumericFieldWidth(theme_sizeType numericFieldWidth);
	void setSplitHandleWidth(theme_sizeType splitHandleWidth);
	void setSubMenuIconHeight(theme_sizeType subMenuIconHeight);
	void setRibbonButtonHeight(theme_sizeType ribbonButtonHeight);
	void setVariablesListTitle(theme_sizeType variablesListTitle);
	void setSliderHandleDiameter(theme_sizeType sliderHandleDiameter);
	void setDefaultTextAreaHeight(theme_sizeType defaultTextAreaHeight);
	void setJaspControlHighlightWidth(theme_sizeType jaspControlHighlightWidth);
	void setDefaultVariablesFormHeight(theme_sizeType defaultVariablesFormHeight);
	void setDefaultSingleItemListHeight(theme_sizeType defaultSingleItemListHeight);
	void setDefaultRectangularButtonHeight(theme_sizeType defaultRectangularButtonHeight);
	void setSmallDefaultVariablesFormHeight(theme_sizeType smallDefaultVariablesFormHeight);
	void setMessageBoxButtonHeight(theme_sizeType messageBoxButtonHeight);
	void setScrollbarBoxWidthBig(theme_sizeType scrollbarBoxWidthBig);
	void setScrollbarBoxWidth(theme_sizeType scrollbarBoxWidth);
	void setMenuItemHeight(theme_sizeType menuItemHeight);
	void setMenuGroupTitleHeight(theme_sizeType menuGroupTitleHeight);
	void setMenuHeaderHeight(theme_sizeType menuHeaderHeight);
	void setHoverTime(theme_timeType hoverTime);
	void setFileMenuSlideDuration(theme_timeType fileMenuSlideDuration);
	void setToolTipDelay(theme_timeType toolTipDelay);
	void setToolTipTimeout(theme_timeType toolTipTimeout);
	void setFont(QFont font);
	void setFontLabel(QFont fontLabel);
	void setFontRibbon(QFont fontRibbon);
	void setFontGroupTitle(QFont fontGroupTitle);
	void setFontPrefOptionsGroupTitle(QFont fontPrefOptionsGroupTitle);
	void setIconPath(QString iconPath);
	void setThemeName(QString themeName);
	void setFontRCode(QFont fontRCode);
	void setFontCode(QFont fontCode);
	void setIsDark(bool isDark);
	void uiScaleHandler();

private:
	void connectSizeDistancesToUiScaleChanged();

private slots:
	void updateFontMetrics();

private:
	static JaspTheme		* _currentTheme;

	float				_ribbonScaleHovered,
						_uiScale				= 1,	///< default for when in R, otherwise ignored
						_maximumFlickVelocity	= 801;	///< default for when in R, otherwise ignored

	QColor				_white,
						_whiteBroken,
						_black,
						_gray,
						_grayDarker,
						_grayLighter,
						_grayMuchLighter,
						_grayVeryMuchLighter,
						_blue,
						_blueDarker,
						_blueLighter,
						_blueMuchLighter,
						_red,
						_redDarker,
						_green,
						_yellowLight,
						_rose,
						_roseLight,
						_cyan,
						_shadow,
						_jaspBlue,
						_jaspGreen,
						_textEnabled,
						_textDisabled,
						_uiBackground,
						_uiBorder,
						_fileMenuColorBackground,
						_fileMenuLightBorder,
						_buttonColor,
						_buttonColorHovered,
						_buttonColorPressed,
						_buttonColorDisabled,
						_buttonBorderColor,
						_buttonBorderColorHovered,
						_itemHighlight,
						_itemHoverColor,
						_itemSelectedColor,
						_itemSelectedNoFocusColor,
						_borderColor,
						_focusBorderColor,
						_dependencyBorderColor,
						_dependencySelectedColor,
						_containsDragBorderColor,
						_analysisBackgroundColor,
						_controlBackgroundColor,
						_controlDisabledBackgroundColor,
						_rowEvenColor,
						_rowOnevenColor,
						_controlErrorBackgroundColor,
						_controlErrorTextColor,
						_controlWarningBackgroundColor,
						_controlWarningTextColor,
						_buttonBackgroundColor,
						_tooltipBackgroundColor,
						_debugBackgroundColor,
						_errorMessagesBackgroundColor,
						_sliderPartOn,
						_sliderPartOff,
						_darkeningColour;

	theme_distanceType	_borderRadius,
						_shadowRadius,
						_itemPadding,
						_jaspControlPadding,
						_ribbonButtonPadding,
						_groupContentPadding,
						_rowSpacing,
						_rowGridSpacing,
						_rowGroupSpacing,
						_columnGridSpacing,
						_columnGroupSpacing,
						_indentationLength,
						_labelSpacing,
						_menuSpacing,
						_menuPadding,
						_generalAnchorMargin,
						_generalMenuMargin,
						_titleBottomMargin,
						_contentMargin,
						_subOptionOffset;

	theme_sizeType		_minPanelWidth,
						_resultWidth,
						_formWidth,
						_iconSize,
						_formMargin,
						_formExpanderHeaderHeight,
						_sliderWidth,
						_sliderLength,
						_switchHeight,
						_spinBoxHeight,
						_spinBoxWidth,
						_comboBoxHeight,
						_textFieldWidth,
						_textFieldHeight,
						_numericFieldWidth,
						_splitHandleWidth,
						_subMenuIconHeight,
						_ribbonButtonHeight,
						_variablesListTitle,
						_sliderHandleDiameter,
						_defaultTextAreaHeight,
						_jaspControlHighlightWidth,
						_defaultVariablesFormHeight,
						_defaultSingleItemListHeight,
						_defaultRectangularButtonHeight,
						_smallDefaultVariablesFormHeight,
						_messageBoxButtonHeight,
						_scrollbarBoxWidthBig,
						_scrollbarBoxWidth,
						_menuItemHeight,
						_menuGroupTitleHeight,
						_menuHeaderHeight;

	//Times: https://www.youtube.com/watch?v=90WDats6eE
	theme_timeType		_hoverTime							=  400,
						_fileMenuSlideDuration				=  150,
						_toolTipDelay						=  500,
						_toolTipTimeout						=60000;

	QFont				_font,
						_fontLabel,
						_fontRibbon,
						_fontRCode,
						_fontCode,
						_fontGroupTitle,
						_fontPrefOptionsGroupTitle;

	QString				_iconPath,
						_themeName;

	bool				_isDark = false;

	static QFontMetricsF					_fontMetrics;
	static std::map<QString, JaspTheme *>	_themes;
};




#endif // JASPTHEME_H
