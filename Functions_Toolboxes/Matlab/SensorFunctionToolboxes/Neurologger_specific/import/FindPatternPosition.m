function [ Position ] = FindPatternPosition( FName, Abyte, Pattern, NbytesToRead )
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here
    AllPositions = true;    %This is addition for multiple start patterns
    AbyteP = Abyte(Pattern);
    fid = fopen(FName, 'r');
    if AllPositions
      A = fread(fid,inf,'uint8=>uint8'); %inf
      NbytesToRead = length(A);
    else
      A = fread(fid,NbytesToRead,'uint8=>uint8');
    end
    fclose(fid);
    AbyteSize = length(Abyte);
    Found = false;
    Position = [];
    Position_t = 0;
    while ((~Found)||AllPositions) && (Position_t+AbyteSize<NbytesToRead)
      At = A(Position_t+Pattern);  
      match = true;
      for I_t =1:length(AbyteP)
        if AbyteP(I_t)~=At(I_t)
          match = false;  
        end    
      end
      if match
          Found = true;
          Position = [Position Position_t];
      end    
      Position_t = Position_t+1;    
    end
    if ~Found; Position=[]; end    
end

