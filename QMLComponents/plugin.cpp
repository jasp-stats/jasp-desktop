// Copyright (C) 2017 The Qt Company Ltd.
// SPDX-License-Identifier: LicenseRef-Qt-Commercial OR BSD-3-Clause

#include <QtQml/QQmlEngineExtensionPlugin>
#include <QQmlEngine>
#include <QQmlContext>
#include "preferencesmodelbase.h"
#include "jasptheme.h"
#include "controls/jaspcontrol.h"
#include <qdebug.h>
#include "knownissues.h"
#include "utilities/qmlutils.h"


//![plugin]
class JASPQmlPlugin : public QQmlEngineExtensionPlugin
{
    Q_OBJECT
    Q_PLUGIN_METADATA(IID QQmlEngineExtensionInterface_iid)


	void initializeEngine(QQmlEngine *engine, const char *uri) override
	{
		QQmlEngineExtensionPlugin::initializeEngine(engine, uri);

		QLocale::setDefault(QLocale(QLocale::English)); // make decimal points == .

		QmlUtils::setGlobalPropertiesInQMLContext(engine->rootContext());

		PreferencesModelBase* prefModel = engine->rootContext()->contextProperty("preferencesModel").value<PreferencesModelBase*>();
		if (prefModel == nullptr)
		{
			prefModel = new PreferencesModelBase();
			engine->rootContext()->setContextProperty("preferencesModel",		prefModel);
		}

		if (engine->rootContext()->contextProperty("jaspTheme").isNull())
		{
			JaspTheme* defaultJaspTheme = new JaspTheme();
			defaultJaspTheme->setIconPath("/default/");
			engine->rootContext()->setContextProperty("jaspTheme",				defaultJaspTheme	);
		}

		qmlRegisterUncreatableMetaObject(JASPControl::staticMetaObject, // static meta object
										 "JASP.Controls",        // import statement
										 0, 1,                   // major and minor version of the import
										 "JASP",                 // name in QML
										 "Error: only enums");
		if (!KnownIssues::issues())
			new KnownIssues(this);

		// TODO: I don't know anymore why I had to add these lines for the pilot project. It does not seem to be needed.
		// ALTNavControl::ctrl()->enableAlTNavigation(prefModel->ALTNavModeActive());
		// connect(prefModel,	&PreferencesModelBase::ALTNavModeActiveChanged,	ALTNavControl::ctrl(),	&ALTNavControl::enableAlTNavigation);

	}
};
//![plugin]

#include "plugin.moc"


