#pragma once

#include <QStringList>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <json/json.h>

// A worker reports application diagnostics separately from its Qt/Chromium log.
struct BatchResult
{
	QStringList errors, warnings;
	static constexpr const char * prefix = "JASP_BATCH_RESULT ";

	void addError(const QString & message)   { append(errors, message); }
	void addWarning(const QString & message) { append(warnings, message); }

	void collect(const Json::Value & result, const QString & analysis)
	{
		if (!result.isObject())
			return;

		if (result.isMember("errorMessage") && result["errorMessage"].isString())
			addError(analysis + ": " + QString::fromStdString(result["errorMessage"].asString()));
		if (result.isMember("error") && result["error"].isObject())
		{
			const auto & error = result["error"];
			const QString message = QString::fromStdString(error.get("errorMessage", "").asString());
			addError(analysis + ": " + (message.isEmpty() ? "Analysis failed" : message));
		}

		// Only visit result objects named in .meta, never table data or user annotations.
		if (result.isMember(".meta") && result[".meta"].isArray())
			for (const auto & meta : result[".meta"])
				if (meta.isObject() && meta["name"].isString() && result.isMember(meta["name"].asString()))
					collect(result[meta["name"].asString()], analysis);
		if (result.isMember("collection") && result["collection"].isObject())
			for (const auto & child : result["collection"])
				collect(child, analysis);
	}

	QByteArray serialize() const
	{
		return QByteArray(prefix) + QJsonDocument(QJsonObject{
			{"errors", QJsonArray::fromStringList(errors)},
			{"warnings", QJsonArray::fromStringList(warnings)}}).toJson(QJsonDocument::Compact);
	}

	bool read(const QByteArray & line)
	{
		if (!line.startsWith(prefix)) return false;
		const auto doc = QJsonDocument::fromJson(line.mid(int(QByteArray(prefix).size())));
		if (!doc.isObject() || !doc["errors"].isArray() || !doc["warnings"].isArray()) return false;
		for (const auto & message : doc["errors"].toArray()) addError(message.toString());
		for (const auto & message : doc["warnings"].toArray()) addWarning(message.toString());
		return true;
	}

private:
	static void append(QStringList & messages, const QString & message)
	{
		if (!message.trimmed().isEmpty() && !messages.contains(message)) messages.append(message);
	}
};
