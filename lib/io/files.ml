let read_file_safe filename =
  try
    let channel = open_in filename in
    let size = in_channel_length channel in
    let content = really_input_string channel size in
    close_in_noerr channel;  
    Some content  
  with
  | Sys_error msg | Failure msg ->  
      print_endline ("Cannot read file: " ^ msg);
      None  