library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;

entity uart is
	port(iclk: in std_logic;
		 wdata: in unsigned (7 downto 0);
		 rdata: out unsigned (7 downto 0);
		 tx: out std_logic:='1';
		 rx: in std_logic;
		 wi: in std_logic;
		 wc: out std_logic:='0';
		 rc: out std_logic:='0');
end uart;

architecture a_uart of uart is
signal txdata: unsigned (7 downto 0) := x"00";
signal rxdata: unsigned (7 downto 0) := x"00";
signal dsize: unsigned (7 downto 0) := x"00";
begin

write: process 
		variable state:integer:=0;
		variable pause:integer:=118;
		variable cnt:integer:=0;
		variable idx:integer:=0;
		
		variable baudrate:integer:=108;		--> 921600 Bauds -> 54 clocks of 50MHz
		--variable baudrate:integer:=434;		--> 115200 Bauds -> 434 clocks of 50MHz 
		
		begin
			if iclk'event and iclk='1' then
				if wi = '1' then
					dsize <= x"08";
					txdata <= wdata;
					wc <= wi;
				end if;
				
			
				if dsize > 0 then
					cnt := cnt + 1;
					
					if(state = 0) then
						if(cnt = pause) then
							cnt := 0;
							state := state + 1;
							pause := 120; 
							
							-- start bit
							tx <= '0';
						end if;
					elsif(state <= 8) then
						if(cnt = pause) then
							cnt := 0;
							state := state + 1;
							pause := baudrate;
							
							-- data
							tx <= txdata(idx);
							idx := idx + 1;
						end if;
					elsif(state = 9) then
						if(cnt = pause) then
							cnt := 0;
							state := state + 1;
							pause := 10;
							
							-- stop bit
							tx <= '1';
						end if;	
					else
						if(cnt = pause) then
							cnt := 0;
							state := 0;
							pause := baudrate;									-- pause between characters
							tx <= '1';
							
							if idx >= 8 then
								dsize <= x"00";
								idx := 0;
								wc <= '0';
							end if;
						end if;	
					end if;
				end if;
			end if;	
	end process;	

read: process 
		variable state:integer:=0;
		variable pause:integer:=0;
		variable cnt:integer:=0;
		variable idx:integer:=0;
		
		variable baudrate:integer:=108;			--> 921600 Bauds -> 54 clocks of 50MHz
		--variable baudrate:integer:=434;		--> 115200 Bauds -> 434 clocks of 50MHz 
		
		begin
		    --if iclk'event and iclk='1' then
		    if rising_edge(iclk) then
				cnt := cnt + 1;	
				if(state = 0) then
					--if falling_edge(rx) then
					--if rx'event and rx='0' then
					if rx='0' then
						cnt := 0;
						state := state + 1;
						pause := 120;				-- 10% overhead to 
						rxdata <= x"00";
						idx := 0;
					end if;	
				elsif(state <= 8) then				-- data bits
					if(cnt > pause) then
						cnt := 0;
						state := state + 1;
						pause := baudrate;
						rxdata(idx) <= rx;
						idx := idx + 1;
					
					end if;
				elsif(state = 9) then				-- stop bit
					if(cnt > pause) then
						cnt := 0;
						state := state + 1;
						pause := 10;
						rdata <= rxdata;
						rc <= '1';	
					end if;	
				elsif(state = 10) then				-- process data on higher level
					if(cnt > pause) then
					
						cnt := 0;
						rc <= '0';
						state := 0;
					end if;	
				end if;
				
			end if;	
	end process;


end a_uart;

