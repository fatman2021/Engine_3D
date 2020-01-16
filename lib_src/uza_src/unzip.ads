--  ___  ____  ____  ____  ________  ___   ______       ______     ___
--  |.|  |../  |...\ |../ /___..._/  |.|   |.___.\     /. __ .\  __|.|   ____
--  |.|  |.|   |.|\.\|.|     /../    |.|   |.____/     |.|__|.| /....|  __\..\
--  |.|__|.|   |.| \...|   _/../___  |.|   |.|    ===  |..__..||. = .| | = ..|
--  |______|  /__|  \__|  /_______/  |_|  /__|        /__|  |_| \__\_|  \__\_|

-- UnZip-Ada
------------
-- Unzips deflated, imploded, reduced, shrunk and stored files

--  Ada translation & cleanup by Gautier de Montmollin
--    http://members.xoom.com/gdemont/unzipada.htm

--  based on Pascal version 2.10 by Dr Abimbola A Olowofoyeku,
--    http://ourworld.compuserve.com/homepages/African_Chief/sources.htm,

--  itself based on Pascal version by Christian Ghisler,
--  itself based on C code by Info-Zip group (Mark Adler et al.)
--    http://www.cdrom.com/pub/infozip/

-- Technical documentation: read appnote.txt

-- Legal note:

--  Copyright (c) 1999 Gautier de Montmollin
--  CH-2000 Neuchatel
--  SWITZERLAND
--  Permission granted to use this library, without any warranty, for any
--  purpose, provided this copyright note remains attached and unmodified.

package Unzip is

  ----------------------------------
  -- Simple extraction procedures --
  ----------------------------------

  procedure Extract( from, what : String;
                     test_only  : boolean:= false ); -- a precise file
  procedure Extract( from, what, rename : String;
                     test_only  : boolean:= false ); -- id., under a new name
  procedure Extract_all_files(
                     from       : String;
                     test_only  : boolean:= false ); -- all files

  -------------------------------------------------------------------------
  -- Simple extraction procedures without re-searching central directory --
  -------------------------------------------------------------------------

  type zip_info is private; -- contains zip file name and its sorted directory

  procedure Load_zip_info( from : String; info : out zip_info );
  procedure Delete_zip_info( info : in out zip_info );

  procedure Extract( from : zip_info; what : String;
                     test_only  : boolean:= false );
  procedure Extract( from : zip_info; what, rename : String;
                     test_only  : boolean:= false );

  ----------------------------------------------
  -- Extraction procedures for user interface --
  ----------------------------------------------

  -- NB: the usage of following accesses may necessitate
  -- the non-standard attribute "unrestricted_access" - or some changes.
  -- Read unzipada.adb for details.

  type feedback_proc is access
    procedure ( percents_done: in natural; user_abort: out boolean );

  type name_conflict_intervention is
    ( yes, no, yes_to_all, none, rename, abort_now );

  current_user_attitude : name_conflict_intervention:= yes;
  -- reset to "yes" for a new session (in case of yes_to_all / none state!)

  type resolve_conflict_proc is access
    procedure ( name            :  in String;
                action          : out name_conflict_intervention;
                new_name        : out String;
                new_name_length : out Natural );

  type pkzip_method is
   ( store,    -- ok
     shrink,   -- ok
     reduce_1, -- ok
     reduce_2, -- ok
     reduce_3, -- ok
     reduce_4, -- ok
     implode,  -- ok
     tokenize, -- not implemented by PKWARE
     deflate,  -- ok
     unknown );

  -- Inform user about some archive data

  type tell_data_proc is access
    procedure ( name               : String;
                compressed_bytes   : long_integer;
                uncompressed_bytes : long_integer;
                method             : pkzip_method );


  procedure Extract( from, what  : String;
                     feedback    : feedback_proc;
                     file_exists : resolve_conflict_proc;
                     tell_data   : tell_data_proc;
                     test_only   : boolean:= false );

  procedure Extract( from, what, rename : String;
                     feedback    : feedback_proc;
                     tell_data   : tell_data_proc;
                     test_only   : boolean:= false );

  procedure Extract_all_files(
                     from        : String;
                     feedback    : feedback_proc;
                     file_exists : resolve_conflict_proc;
                     tell_data   : tell_data_proc;
                     test_only   : boolean:= false );

  procedure Extract( from : zip_info; what : String;
                     feedback    : feedback_proc;
                     file_exists : resolve_conflict_proc;
                     tell_data   : tell_data_proc;
                     test_only   : boolean:= false );

  procedure Extract( from : zip_info; what, rename : String;
                     feedback   : feedback_proc;
                     tell_data   : tell_data_proc;
                     test_only  : boolean:= false );

  function Find_offset(info: zip_info; name: String) return long_integer;

private

  -- zip_info, 23.VI.1999.

  -- The PKZIP central directory is coded here as a binary tree
  -- to allow a fast retrieval of the searched offset in zip file.
  -- E.g. for a 1000-file archive, the offset will be found in less
  -- than 11 moves: 2**10=1024 (balanced case), without any read
  -- in the archive.

  type dir_node;
  type p_dir_node is access dir_node;

  type dir_node(name_len: natural) is record
    left, right : p_dir_node;
    name        : string(1..name_len);
    offset      : long_integer;
  end record;

  type p_string is access string;

  type zip_info is record
    loaded          : boolean:= False;
    zip_file_name   : p_string;
    dir_binary_tree : p_dir_node;
  end record;

end Unzip;
