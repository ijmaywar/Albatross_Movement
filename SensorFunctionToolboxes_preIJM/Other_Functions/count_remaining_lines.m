function numlines = count_remaining_lines(fid);
   numlines = 0;
   while true
     if ~ischar(fgets(fid)); break; end    %end of file
     numlines = numlines + 1;
   end
 end