struct Tag {
	const char* start;
	const char* stop;
	const char* start_end;
	const char* stop_end;
	const char* p;
	bool inside;
	bool is_on_start_tag;
	bool is_on_stop_tag;

	Tag(const char* start, const char* stop);

	void reset();

	bool isInside();

	bool isOnStartTag();

	bool isOnStopTag();

	void advance(char c);
};


