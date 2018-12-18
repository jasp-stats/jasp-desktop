#ifndef FILEMENULISTITEM_H
#define FILEMENULISTITEM_H

#include <QAbstractListModel>
#include <QHash>

enum FileMenuListItemType {
	NameRole = Qt::UserRole,
	PathRole,
	DescriptionRole,
	TypeRole,
	AssociatedDataFileRole,
	IconSourceRole,
	DataIconSourceRole,
	DirRole
};

const QHash<int, QByteArray> FileMenuListItemTypeRoleNames = {
	{ NameRole,					"name"					},
	{ DescriptionRole,			"description"			},
	{ PathRole,					"path"					},
	{ TypeRole,					"type"					},
	{ AssociatedDataFileRole,	"associated_datafile"	},
	{ IconSourceRole,			"iconsource"			},
	{ DataIconSourceRole,		"dataiconsource"		},
	{ DirRole,					"dirpath"				}
};

#endif // FILEMENULISTITEM_H
