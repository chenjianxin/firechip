#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

class InputStream {
  public:
    InputStream(const char *filename);
    ~InputStream(void);

    bool out_valid() { return !complete; }
    uint64_t out_bits() { return data; }
    void tick(bool out_ready);

  private:
    void read_next(void);
    bool complete;
    FILE *file;
    uint64_t data;
};

InputStream::InputStream(const char *filename)
{
    this->file = fopen(filename, "r");
    if (this->file == NULL) {
        fprintf(stderr, "Could not open %s\n", filename);
        abort();
    }

    read_next();
}

InputStream::~InputStream(void)
{
    fclose(this->file);
}

void InputStream::read_next(void)
{
    int res = fread(&this->data, sizeof(uint64_t), 1, this->file);
    if (res < 0) {
        perror("fread");
        abort();
    }

    this->complete = (res == 0);
}

void InputStream::tick(bool out_ready)
{
    int res;

    if (out_valid() && out_ready)
        read_next();
}

InputStream *stream = NULL;

extern "C" void input_stream_init(const char *filename)
{
    stream = new InputStream(filename);
}

extern "C" void input_stream_tick(
        unsigned char *out_valid,
        unsigned char out_ready,
        long long     *out_bits)
{
    stream->tick(out_ready);
    *out_valid = stream->out_valid();
    *out_bits  = stream->out_bits();
}
