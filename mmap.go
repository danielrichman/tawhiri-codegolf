package main

/*
#include <sys/mman.h>
*/
import "C"

import (
    "os"
    "errors"
    "unsafe"
)

func mmap(file *os.File) (unsafe.Pointer, int64, error) {
    fi, err := file.Stat()
    if err != nil {
        return nil, 0, err
    }
    size := fi.Size()

    data, _ := C.mmap(nil, C.size_t(size), C.PROT_READ, C.MAP_SHARED,
                      C.int(file.Fd()), 0)
    if data == nil {
        // do something with errno
        return nil, 0, errors.New("map failed")
    }

    // todo: do something with runtime.setFinaliser to unmap.
    return data, size, nil
}
