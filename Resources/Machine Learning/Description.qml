import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	title :			qsTr("Machine Learning")
	name :			"Machine Learning"
	description:	qsTr("A module for Machine Learning mastery")
	version:		"0.1"
	author:			"JASP Team"
	maintainer:		"JASP Team <info@jasp-stats.org>"
	website:		"www.jasp-stats.org"
	license:		"GPL (>= 2)"
	icon:			"analysis-ml-ribbon.svg"

	Package { name: "AUC"			}
	Package { name: "cluster"		}
	Package { name: "colorspace"	}
	Package { name: "dbscan"		}
	Package { name: "e1071"			}
	Package { name: "gbm"			}
	Package { name: "ggdendro"		}
	Package { name: "kknn"			}
	Package { name: "randomForest"	}
	Package { name: "ROCR"			}
	Package { name: "Rtsne"			}

	GroupTitle
	{
		title:	qsTr("Regression")
		icon: 	"analysis-ml-regression.svg"
	}
	Analysis
	{
		menu:	qsTr("Boosting")
		title:	qsTr("Boosting Regression")
		func:	"mlRegressionBoosting"
	}
	Analysis
	{
		menu:	qsTr("K-Nearest Neighbors")
		title:	qsTr("K-Nearest Neighbors Regression")
		func:	"mlRegressionKnn"
	}
	Analysis
	{
		menu:	qsTr("Random Forest")
		title:	qsTr("Random Forest Regression")
		func:	"mlRegressionRandomForest"
	}
	Analysis
	{
		menu:	qsTr("Regularized Linear")
		title:	qsTr("Regularized Linear Regression")
		func:	"mlRegressionRegularized"
	}


	GroupTitle
	{
		title:	qsTr("Classification")
		icon: 	"analysis-ml-classification.svg"
	}
	Analysis
	{
		menu:	qsTr("Boosting")
		title:	qsTr("Boosting Classification")
		func:	"mlClassificationBoosting"
	}
	Analysis
	{
		menu:	qsTr("K-Nearest Neighbors")
		title:	qsTr("K-Nearest Neighbors Classification")
		func:	"mlClassificationKnn"
	}
	Analysis
	{
		menu:	qsTr("Linear Discriminant")
		title:	qsTr("Linear Discriminant Classification")
		func: 	"mlClassificationLda"
	}
	Analysis
	{
		menu:	qsTr("Random Forest")
		title:	qsTr("Random Forest Classification")
		func:	"mlClassificationRandomForest"
	}


	GroupTitle
	{
		title:	qsTr("Clustering")
		icon: 	"analysis-ml-clustering.svg"
	}
	Analysis
	{
		menu:	qsTr("Density-Based")
		title:	qsTr("Density-Based Clustering")
		func:	"mlClusteringDensityBased"
	}
	Analysis
	{
		menu:	qsTr("Fuzzy C-Means")
		title:	qsTr("Fuzzy C-Means Clustering")
		func:	"mlClusteringFuzzyCMeans"
	}
	Analysis
	{
		menu:	qsTr("Hierarchical")
		title:	qsTr("Hierarchical Clustering")
		func:	"mlClusteringHierarchical"
	}
	Analysis
	{
		menu:	qsTr("K-Means")
		title:	qsTr("K-Means Clustering")
		func:	"mlClusteringKMeans"
	}
	Analysis
	{
		menu:	qsTr("Random Forest")
		title:	qsTr("Random Forest Clustering")
		func:	"mlClusteringRandomForest"
	}
}
