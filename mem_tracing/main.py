import json
import sys

print_extra = 0

def convert_txt_to_chrome_trace(input_file: str, output_file: str):
    previous_seen_block = 0
    block_base = 0
    offset = 0
    trace_events = []
    type_allocations = {}
    type_first_seen = {}

    block_number = 0

    print_extra = 0

    with open(input_file, 'r') as f:
        header = f.readline().strip()

        for line in f:
            if not line.strip():
                continue

            parts = line.strip().split('|')

            seq = int(parts[0])
            current_block = parts[1]
            block_size = int(parts[2])
            main_offset = int(parts[3])
            size = int(parts[4])
            extra = int(parts[5])
            alloc_type = parts[6]
            ptr = parts[7]
            phase = parts[8]
            row = parts[9]
            col = parts[10]

            new_block_detected = False
            if(previous_seen_block != current_block):
                previous_seen_block = current_block
                new_block_detected = True
                block_number += 1


            if block_number == 1:
                offset = main_offset
            else:
                offset = block_size * (block_number - 1) + main_offset


            extra_base = offset - (extra - size)


            if alloc_type not in type_first_seen:
                type_first_seen[alloc_type] = seq

            if new_block_detected:
                trace_events.append({
                    "name": f"{current_block}: {block_number}",
                    "ph": "X",
                    "ts": block_base,
                    "dur": block_size,
                    "pid": 1,
                    "tid": 3,
                    "args": {
                        "block": current_block,
                        "block_base": block_base,
                        "block_size": block_size
                    }
                })

                block_base += block_size
                new_block_detected = False

            # for block_number 1 we use the main_offset but can't use it for first sequence because it will be non zero
            if seq == 1:
                offset = 0

            # Allocation by type threads
            if alloc_type not in type_allocations:
                type_allocations[alloc_type] = []

            type_allocations[alloc_type].append({
                'offset': offset,
                'size': size,
                'seq': seq,
                'ptr': ptr,
                'phase': phase,
                'row': row,
                'col': col

            })


            # Main thread
            trace_events.append({
                "name": alloc_type,
                "ph": "X",
                "ts": offset,
                "dur": size,
                "pid": 1,
                "tid": 1,
                "args": {
                    "type": alloc_type,
                    "offset": offset,
                    "size": size,
                    "phase": phase,
                    "seq": seq,
                    "ptr": ptr,
                    "row": row,
                    "col": col
                }
            })

            # Phases thread
            if phase:
                trace_events.append({
                    "name": phase,
                    "ph": "X",
                    "ts": offset,
                    "dur": size,
                    "pid": 1,
                    "tid": 2,
                    "args": {
                        "phase": phase,
                        "offset": offset,
                        "type": alloc_type,
                        "seq": seq
                    }
                })

            # Extra bytes thread (padding)
            if extra != 0:
                print_extra += extra
                trace_events.append({
                    "name": "extra",
                    "ph": "X",
                    "ts": extra_base,
                    "dur": extra,
                    "pid": 1,
                    "tid": 4,
                    "args": {
                        "offset": offset,
                        "size": size,
                        "extra": extra
                    }
                })



    type_thread_map = {}
    for i, alloc_type in enumerate(type_first_seen):
        type_thread_map[alloc_type] = 10 + i

    for alloc_type, allocations in type_allocations.items():
        thread_id = type_thread_map[alloc_type]
        for alloc in allocations:
            trace_events.append({
                "name": alloc_type,
                "ph": "X",
                "ts": alloc['offset'],
                "dur": alloc['size'],
                "pid": 2,
                "tid": thread_id,
                "args": {
                    "type": alloc_type,
                    "size": alloc['size'],
                    "phase": alloc['phase'],
                    "seq": alloc['seq'],
                    "ptr": alloc['ptr'],
                    "row": alloc['row'],
                    "col": alloc['col']
                }
            })

    # per thread info
    trace_events.append({"name": "process_name", "ph": "M", "pid": 1, "args": {"name": "Memory Allocations"}})
    trace_events.append({"name": "thread_name", "ph": "M", "pid": 1, "tid": 1, "args": {"name": "Overview"}})
    trace_events.append({"name": "thread_name", "ph": "M", "pid": 1, "tid": 2, "args": {"name": "Phases"}})
    trace_events.append({"name": "thread_name", "ph": "M", "pid": 1, "tid": 3, "args": {"name": "Memory Blocks"}})
    trace_events.append({"name": "thread_name", "ph": "M", "pid": 1, "tid": 4, "args": {"name": "cstr padding"}})


    trace_events.append({"name": "process_name", "ph": "M", "pid": 2, "args": {"name": "Allocations by Type"}})
    for alloc_type in type_first_seen:
        trace_events.append({"name": "thread_name", "ph": "M", "pid": 2, "tid": type_thread_map[alloc_type], "args": {"name": alloc_type}})

    with open(output_file, 'w') as f:
        json.dump(trace_events, f, indent=2)

    for t in type_first_seen:
        print(f"{t}: {len(type_allocations[t])} allocations")

    print(f"\nTotal Types: {len(type_first_seen)}")

    print(f"\nOutput: {output_file}")



def main():
    if len(sys.argv) != 2:
        print(f"Error, usage is: {sys.argv[0]} trace.txt")
        sys.exit(1)

    input_path = sys.argv[1]
    output_path = input_path.rsplit('.', 1)[0] + '.json'

    convert_txt_to_chrome_trace(input_path, output_path)
    print(f"Total extra bytes (padding) = {print_extra}")

if __name__ == "__main__":
    main()