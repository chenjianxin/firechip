#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

class InputStream {
  public:
    InputStream(const char *filename, int nbytes);
    ~InputStream(void);

    bool out_valid() { return !complete; }
    uint64_t out_bits() { return data; }
    void tick(bool out_ready);

  private:
    void read_next(void);
    bool complete;
    FILE *file;
    int nbytes;
    uint64_t data;
};

InputStream::InputStream(const char *filename, int nbytes)
{
    this->nbytes = nbytes;
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
    int res;

    this->data = 0;

    res = fread(&this->data, this->nbytes, 1, this->file);
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

extern "C" void input_stream_init(const char *filename, int data_bits)
{
    stream = new InputStream(filename, data_bits/8);
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
