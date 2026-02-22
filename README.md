# sort

In-place sorting on arrays.

## API

- `sort_int(arr)` — sort an integer array in ascending order
- `sort_with(arr, cmp)` — sort an array using a custom comparator closure

Uses quicksort with insertion sort for small partitions.

## Dependencies

- array
- arith
