
Guide to adding a module in JASP
=======================

The steps are as follows:

 - Add a tab in tabbar
 - Create the user interface for the ribbon
 - Add the widget in the mainwindow ui
 - connect ribbon with itemSelected()
 - Add a toggle function in tabbar.cpp and connect to slot
 - Create the analysis form
 - Add form to mainwindow

Add a tab in `tabbar.cpp`
------------------
The dropdown menu (widget) showing the various modules has been implemented using the QMenu class,

    //Options
    QMenu *optionmenu = new QMenu("Modules",this);
    QAction *sem = new QAction("SEM", optionmenu);
    QAction *rei = new QAction("Reinforcement Learning", optionmenu);
    QAction *summaryStats = new QAction("Summary Stats",optionmenu);

Add an action to the optionmenu after this,

    QAction *newModule = new QAction("New module", optionmenu);

Following this, properties have to be set to the newModule object.

    // New module
    QVariant newModule_setting = _settings.value("toolboxes/newModule", false);
    newModule->setObjectName("New module");
    newModule->setCheckable(true);
    newModule->setChecked(newModule_setting.canConvert(QVariant::Bool) && newModule_setting.toBool());
    optionmenu->addAction(newModule);

On building the project, a new module is added to the tabbar

Create the interface for the 'New module' ribbon
---------------------------------------
The ribbon interface should be created in `JASP-DESKTOP/ribbons/` - {{ribbonnewmodule}}.cpp, .h, .ui. To make these files discoverable by Qt, they should be 'added' in the JASP-Desktop.pri file

    $$PWD/ribbons/ribbonsummarystatistics.ui \
    $$PWD/ribbons/ribbonnewmodule.ui \

Suppose the New module ribbon has a button, 'Analyses'. If these analyses do not need a dataset to work with, add the following line in the `ribbonnewmodule.cpp` file,

	ui->analysesButton->setDataSetNotNeeded();

To add a dropdown list with different options (here two options Test Analysis 1 and 2 are created), create a QMenu,

    QMenu *menu;
    menu = new QMenu(this);
    menu->addAction(QString("Test Analysis 1"), this, SLOT(itemSelected()))->setObjectName("TestAnalysis1");
    menu->addAction(QString("Test Analysis 2"), this, SLOT(itemSelected()))->setObjectName("TestAnalysis2");
    ui->analysesButton->setMenu(menu);

Modify the MainWindow component
-----------------------------------
##### Promote - new module ribbon
Open the mainwindow.ui in the QtEditor, click on the `ribbon` component in the Object inspector. ribbon -> Insert Page -> After Current Page (It should be added after the last page). Change the name to ribbonNewModule (naming convention). ribbonNewModule is a QWidget object. Select the object and click **Promote to ...** option from the context menu. After entering the class name (RibbonNewModule) and header file (ribbons/ribbonnewmodule.h) in the lower part of the dialog, choose **Add**. The placeholder class will now appear along with the base class in the upper list. Click the **Promote** button to accept this choice.

##### Connect ribbon with itemSelected()

Connect the ui ribbon component signal with the `itemSelected()` slot in the MainWindow constructor.

	connect(ui->ribbonR11tLearn, SIGNAL(itemSelected(QString)), this, SLOT(itemSelected(QString)));
	connect(ui->ribbonSummaryStatistics, SIGNAL(itemSelected(QString)), this, SLOT(itemSelected(QString)));
	// insert the below line
    connect(ui->ribbonNewModule, SIGNAL(itemSelected(QString)), this, SLOT(itemSelected(QString)));

Toggle function for select/un-select tab
-------------------

##### Add a toggle slot in tabbar.cpp and connect it to triggered() signal

Implement the toggle slot which adds/removes the new module tab.

    void TabBar::toggleNewModule() {
    	QVariant newModule_setting = _settings.value("toolboxes/newModule", false);
    	static bool on = (newModule_setting.canConvert(QVariant::Bool) && newModule_setting.toBool());
    	on = ! on;
        if (on) {
    		this->addTab("New Module");
        } else {
    		this->removeTab("New Module");
        }
    }

and connect the above slot with the triggered() signal in the `addHelpTab()` function

	connect(newModule, SIGNAL(triggered()), this, SLOT(toggleNewModule()));


##### Modify the tabChanged function in MainWindow.cpp
`tabChanged()` in MainWindow.cpp contains an if-else ladder for the current Active tab. Add 'New Module' to it.

Create the analysis form
--------------------

Add form to the MainWindow file
--------------------
Add the analysis to the if else ladder in the loadForm() functon in MainWindow
