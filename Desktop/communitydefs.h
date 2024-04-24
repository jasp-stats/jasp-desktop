#ifndef COMMUNITYDEFS_H
#define COMMUNITYDEFS_H

#include "QStringList"

namespace Coop
{

const QStringList & goldTier()
{
	// Want to thank an institution on the welcomepage?
	// Just add an entry in the list below:
	static QStringList list =
	{
		"University of Amsterdam",
		"M&S / Faculty of Social Sciences / Utrecht University",
		"Nyenrode Business University",
		"KU Leuven",
		"Tilburg University",
	};

	return list;
}

const QStringList & silverTier()
{
	// Want to thank an institution on the welcomepage?
	// Just add an entry in the list below:
	static QStringList list =
	{
		"The College of Arts and Sciences and the Department of Statistics at Texas A&M University",
		"Department of Pediatrics, MosaKids Children’s Hospital, Maastricht University Medical Centre (MUMC+)",
	};

	return list;
}

const QStringList & bronzeTier()
{
	// Want to thank an institution on the welcomepage?
	// Just add an entry in the list below:
	static QStringList list =
	{
		"Vrije Universiteit Amsterdam – EMFC/RC program",
		"NSDSK Academy (The Dutch Foundation for the Deaf and Hard of Hearing Child)",
		"The Department of Communication at Appalachian State University"
	};

	return list;
}

const QString & howToSupport()
{
	static QString howTo = "https://jasp-stats.org/community-how-to-join/";
	return howTo;
}

const QString & communityUrl()
{
	static QString coop = "https://jasp-stats.org/community-vision-and-goals/";
	return coop;
}

const QString & communityMembersUrl()
{
	static QString coop = "https://jasp-stats.org/community-institutional-members/";
	return coop;
}


}
#endif // COMMUNITYDEFS_H
