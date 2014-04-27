
#include "base64.h"

using namespace std;

string Base64::encode(const string &prefix, const string &in)
{
	base64_encodestate state;
	base64_init_encodestate(&state);

	int triplets = in.length() / 3;
	if (in.length() % 3)
		triplets++;

	char buffer[prefix.size() + 4 * triplets];

	int count;
	for (count = 0; count < prefix.length(); count++)
		buffer[count] = prefix.c_str()[count];

	count += base64_encode_block(in.c_str(), in.length(), &buffer[count], &state);
	count += base64_encode_blockend(&buffer[count], &state);

	return string(buffer, count);
}
