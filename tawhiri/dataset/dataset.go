package dataset

import (
    "os"
    "path"
    "time"
    "unsafe"
)

type array [65][47][3][361][720]float64
const ds_size = int64(unsafe.Sizeof(array{}))

const (
    height = 0
    windu = 1
    windv = 2
)

type Dataset struct {
    Array *array
    When time.Time
}

func init() {
    if ds_size != 19057334400 {
        panic("dataset array type has incorrect size")
    }
}

func filename(when time.Time, directory string) string {
    return path.Join(directory, when.Format("2006010215"))
}

func Open(when time.Time, directory string) Dataset {
    // if when != when.Truncate(time.Hour * 3) || when.Location() != time.UTC {
    //    panic("invalid dataset time")
    // }

    file, err := os.Open(filename(when, directory))
    if err != nil {
        panic(err)
    }

    data, size, err := mmap(file)
    if err != nil {
        panic(err)
    }
    if size != ds_size {
        panic("dataset incorrect size")
    }

    return Dataset{(*array)(data), when}
}
