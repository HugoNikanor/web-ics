#include <stdlib.h>
#include <libical/ical.h>

void parse_text (char* argv[]);

int main (int argc, char* argv[]) {
	char* args[1] = { [0] = "/home/hugo/.calendars/D3.b/02b5e538426ee9cd5429c5c6a33d33870fa02380bd3c475812be46991100363e.ics" };
	parse_text (args);
	return 0;
}

char* read_stream (char* s, size_t size, void* d) {
	return fgets (s, (int) size, (FILE*) d);
}

void parse_text (char* argv[]) {
	char* line;
	FILE* stream;
	icalcomponent* c;

	icalparser* parser = icalparser_new();
	stream = fopen(argv[1], "r");

	assert (stream != 0);

	icalparser_set_gen_data (parser, stream);

	do {
		line = icalparser_get_line (parser, read_stream);
		c = icalparser_add_line (parser, line);

		if (c != 0) {
			char* temp = icalcomponent_as_ical_string_r (c);
			printf("%s", temp);
			free(temp);

			printf("\n--------------------\n");
			icalcomponent_free (c);
		}
	} while (line != 0);

	icalparser_free (parser);
}
