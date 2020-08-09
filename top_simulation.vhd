library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;

entity top is
	Port (dtx : out std_logic;														-- uart tx pin	
		  drx : in std_logic;														-- uart rx pin	
		  led: out std_logic_vector (7 downto 0) := x"00";
		  fclk: in  std_logic 														-- fpga clock pin
	     );		
end top;

architecture arc_top of top is

-- Instancing of uart Component
component uart
	port( iclk: in std_logic;
			wdata: in unsigned (7 downto 0);
			rdata: out unsigned (7 downto 0);
			tx: out std_logic:='1';
			rx: in std_logic;
			wi: in std_logic;
			wc: out std_logic;
			rc: out std_logic);
end component;

-- Signals used in arc_top architecture of top entity
-- Signals note from professor: "signal assignment at the end of process iteration!"

-- Signal for tweaking number of Coefficients(It is odd number) MAX VALUE defined by buffer size. Our use case: 63
-- Warning: Also limiting parameter is number of bits in res,sub and tmp signals. 
constant number_coeff_human_friendly: unsigned (7 downto 0) := to_unsigned(3,8);
constant number_coeff: unsigned (7 downto 0) := number_coeff_human_friendly-to_unsigned(1,8); -- Buffer starting from 0

-- Signals for UART
signal urxstate: integer := 0;
signal utxstate: std_logic;
signal usize: unsigned (7 downto 0) := x"00";
signal uwdata: unsigned (7 downto 0) := x"00";
signal urdata: unsigned (7 downto 0) := x"00";
signal uwi: std_logic := '0';
signal uwc: std_logic;
signal urc: std_logic;
signal rxsrc: std_logic := '0';
signal txsrc: std_logic := '0';

-- Signals for Filter 32 bits
signal maf_sub: unsigned (31 downto 0) := x"00000000";
signal maf_res: unsigned (31 downto 0) := x"00000000";
signal maf_tmp: unsigned (31 downto 0) := x"00000000";

-- New type maf_buff_t array of 64 elements of unsigned type each having 32 bits
type maf_buff_t is array (0 to 63) of unsigned (31 downto 0);

-- Instance of new type maf_buff_t and set all values to zero
signal maf_buffer: maf_buff_t :=  (others=> ( others=>'0' ) );

-- Tracking id position of maf_buffer
signal maf_bidx: integer := 0;
signal maf_size: integer := to_integer(number_coeff) + 1;		-- filter size +1

begin
	-- Mapping signals from top entity to instance of uart compoment
	inst_uart: uart port map (iclk=>fclk, wdata=>uwdata, rdata=>urdata, tx=>dtx, rx=>drx, wi=>uwi, wc=>uwc, rc=>urc);
	
	-- FPGA Clock -> compute state of utxstate signal
	utxstate <= rxsrc xor txsrc;

	uart_tx: process
	begin
		if rising_edge(fclk) then
			if utxstate = '1' then	
				uwi <= '1';
				txsrc <= txsrc xor '1';
			end if;
			if uwc = '1' then				
				uwi <= '0';
			end if;	
		end if;
	end process;


	uart_rx: process
	variable maf_div: unsigned (15 downto 0) := x"0000";
	
	begin
		if rising_edge(fclk) then
			if urxstate = 0 then
				if urc = '1' then
					(f)					-- remove oldest when buffer is full then after next iteration starts!
						maf_tmp <= maf_res - maf_sub;						
						--maf_buffer(maf_bidx) <= resize(urdata, 32);
						maf_buffer(maf_bidx)(7 downto 0) <= urdata;		
						urxstate <= urxstate + 1;
					else
						maf_buffer(maf_bidx)(7 downto 0) <= urdata;		
						urxstate <= urxstate + 1;
					end if;
				end if;
			elsif urxstate = 1 then
				if maf_bidx < number_coeff then
					maf_res <= maf_tmp + maf_buffer(maf_bidx) ;				-- Filling buffer only
					urxstate <= urxstate + 2;	
				else
					maf_res <= maf_tmp + maf_buffer(maf_bidx) ;				-- add newest!
					urxstate <= urxstate + 1;
				end if;				
			elsif urxstate = 2 then	
				-- Position of maf_buff array +1
				maf_bidx <= maf_bidx + 1;
				
				maf_div := maf_res(7 downto 0)*number_coeff_human_friendly; --Division by multiplication and shifting
				uwdata <= unsigned(maf_div(15 downto 8));
				
				urxstate <= urxstate + 1;
				
			elsif urxstate = 3 then	
				rxsrc <= rxsrc xor '1';
				
				if(maf_bidx >= maf_size) then
					maf_bidx <= 0;
				end if;
				
				urxstate <= urxstate + 1;
					
			elsif urxstate = 4 then
				if urc = '0' then
					maf_sub <= maf_buffer(maf_bidx);
					urxstate <= 0;
				end if;
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
