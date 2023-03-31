#define ENUM_DECLARATION_CPP
#include "enginedefinitions.h"

const char * unexpectedEngineReply::what() const noexcept{ return std::runtime_error::what(); }

void unexpectedEngineReply::checkIfExpected(engineState expectedReplyState, engineState currentState, int channelNo)
{
	if(expectedReplyState != currentState)
		throw unexpectedEngineReply(expectedReplyState, channelNo, ", in state: " + engineStateToString(currentState));
}
