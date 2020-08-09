library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;

entity top is
	Port (dtx : out std_logic;														-- uart tx pin	
		  drx : in std_logic;														-- uart rx pin	
		  led: out unsigned (7 downto 0) := x"00";
		  fclk: in  std_logic; 														-- fpga clock pin
		  urdata: in unsigned(7 downto 0) := x"00";					-- Simulation purpose only
		  writeData: in std_logic
	     );		
end top;

architecture arc_top of top is

-- Signals for UART
signal urxstate: integer := 0;

signal maf_sub: unsigned (31 downto 0) := x"00000000";
signal maf_res: unsigned (31 downto 0) := x"00000000";
signal maf_tmp: unsigned (31 downto 0) := x"00000000";
type maf_buff_t is array (0 to 63) of unsigned (31 downto 0);
signal maf_buffer: maf_buff_t :=  (others=>(others=>'0'));

constant maf_smpls: unsigned(15 downto 0) := x"0003"; 						--sadasd
constant maf_smpls_it: unsigned(15 downto 0) := maf_smpls - x"01";
signal maf_bidx: integer := 0;
signal maf_size: integer := to_integer(maf_smpls_it);			    		-- filter size +1
signal el_buff: integer := 0;
signal dumper: unsigned(47 downto 0) := to_unsigned(0,48);

begin
	data_rx: process(fclk)
	variable maf_multp: unsigned(47 downto 0) := to_unsigned(0,48);
	
	begin
		if rising_edge(fclk) then
			if urxstate = 0 then
				if writeData = '1' then
					if el_buff <= maf_size then
						maf_buffer(maf_bidx)(7 downto 0) <= urdata;
						urxstate <= urxstate + 1;
						maf_tmp <= maf_res;
						el_buff <= el_buff + 1;
					else
						maf_tmp <= maf_res - maf_sub;						-- remove oldest!
						--maf_buffer(maf_bidx) <= resize(urdata, 32);
						maf_buffer(maf_bidx)(7 downto 0) <= urdata;		

						urxstate <= urxstate + 1;
					end if;
				end if;
			elsif urxstate = 1 then
				maf_res <= maf_tmp + maf_buffer(maf_bidx);				-- add newest!
				urxstate <= urxstate + 1;
			elsif urxstate = 2 then	
				if el_buff >= maf_size then
					maf_bidx <= maf_bidx + 1;
					
					--int1 := to_integer(maf_res);
					--int2 := to_integer(maf_smpls);
					--res := int1/int2;
					--tmpres := conv_unsigned(res, 32);

					maf_multp := shift_right(maf_res*to_unsigned(65536/to_integer(maf_smpls), maf_smpls'length),16);
					dumper <= maf_res*to_unsigned(65536/to_integer(maf_smpls), maf_smpls'length);
					led <= maf_multp(7 downto 0);	-- division approximation
					
					urxstate <= urxstate + 1;
				else 
					maf_bidx <= maf_bidx + 1;
					led <= to_unsigned(urxstate,8);
					urxstate <= urxstate + 1;
				end if;			
			elsif urxstate = 3 then	
				--rxsrc <= rxsrc xor '1';
				
				if(maf_bidx > maf_size) then
					maf_bidx <= 0;
				end if;
				
				urxstate <= urxstate + 1;
					
			elsif urxstate = 4 then
				--if urc = '0' then
					maf_sub <= maf_buffer(maf_bidx);
					urxstate <= 0;
				--end if;
			end if;
		end if;
	end process;

end arc_top;


-- NOTE:
-- signal vs variable 
-- 1. signal assignment at the end of process iteration!
-- 2. variable assignment can happen before end of process iteration! -> be careful here!
--    you can easily hit timing issue!
--for i in 0 to maf_size loop
	--maf_res  := maf_res + maf_buffer(i);	
--end loop;	