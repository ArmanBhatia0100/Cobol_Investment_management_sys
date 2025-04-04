# Investment Report Generator

## Overview
This COBOL program, `INVESTMENT-REPRT`, generates a financial report based on stock portfolio data. It reads stock information and investment records from input files, processes the data to calculate gains/losses, and produces a formatted report showing key investment metrics.

## Features
- Reads stock data (symbol, name, closing price) from `STOCKS.txt`.
- Reads portfolio data (symbol, shares owned, average cost) from `PORTFOLIO.txt`.
- Matches investments with stock data and calculates:
  - Cost basis (shares × average cost)
  - Market value (shares × closing price)
  - Gain/loss (market value - cost basis)
- Outputs a detailed report to `REPORT-OUT.txt` with formatted columns for:
  - Stock name
  - Number of shares
  - Unit cost
  - Closing price
  - Cost basis
  - Market value
  - Gain/loss
- Includes a header, separator lines, and summary of records processed.

## Prerequisites
- A COBOL compiler (e.g., GnuCOBOL or IBM Enterprise COBOL).
- A mainframe or compatible environment if using native COBOL runtime.
- Input files (`STOCKS.txt` and `PORTFOLIO.txt`) in the correct format.

## File Structure
### Input Files
1. **`STOCKS.txt`**  
   - Format: Line-sequential  
   - Record Structure:
     - Stock Symbol (7 chars)
     - Stock Name (25 chars)
     - Closing Price (6 digits, 4 before decimal, 2 after, e.g., 0123.45)  
   - Example:
     ```
     AAPL   Apple Inc.               012345
     MSFT   Microsoft Corporation    009875
     ```

2. **`PORTFOLIO.txt`**  
   - Format: Line-sequential  
   - Record Structure:
     - Investment Symbol (7 chars)
     - Shares Owned (5 digits)
     - Average Cost (6 digits, 4 before decimal, 2 after, e.g., 0123.45)  
   - Example:
     ```
     AAPL   01000 005067
     MSFT   00500 008900
     ```

### Output File
- **`REPORT-OUT.txt`**  
  - Format: Line-sequential  
  - Contains a header, data rows, and a summary of records read/written.  
  - Example Output:
    ```
    STOK NAME                 #SHARES   UNIT COST   AT CLOSING    C0ST BASE     MARKET VALUE    GAIN/LOES
    ============================================================================================================================
    Apple Inc.                1,000     $50.67      $123.45       $50,670.00    $123,450.00     $72,780.00
    Microsoft Corporation       500     $89.00      $98.75        $44,500.00     $49,375.00      $4,875.00
    ============================================================================================================================
    Records read: 002   Records written: 002
    ```

## Installation
1. Clone the repository:
   ```bash
   git clone https://github.com/[your-username]/investment-report-generator.git
   ```
2. Ensure the input files (`STOCKS.txt` and `PORTFOLIO.txt`) are placed in the parent directory relative to the program (or adjust the file paths in the `FILE-CONTROL` section).
3. Compile the COBOL program:
   ```bash
   cobc -x INVESTMENT-REPRT.cbl
   ```

## Usage
1. Run the compiled program:
   ```bash
   ./INVESTMENT-REPRT
   ```
2. Check the output file `REPORT-OUT.txt` in the parent directory for the generated report.

## Program Structure
- **IDENTIFICATION DIVISION**: Defines the program name (`INVESTMENT-REPRT`).
- **ENVIRONMENT DIVISION**: Configures input/output files.
- **DATA DIVISION**: Defines file structures, working storage, and report layout.
- **PROCEDURE DIVISION**: Contains the main logic:
  - `INITIALIZATION`: Opens files and loads stock data into a table.
  - `PROCESS-RECORDS`: Reads portfolio records and processes each investment.
  - `PROCESS-INVESTMENT-RECORD`: Calculates financial metrics and writes report lines.
  - `TERMINATION`: Closes files and writes summary.

## Limitations
- Supports up to 20 unique stocks in the stock table (adjustable via `OCCURS` clause).
- Assumes input files are correctly formatted and exist in the specified paths.
- No error handling for missing or malformed data.

## Contributing
Feel free to fork this repository and submit pull requests for enhancements, such as:
- Adding error handling for invalid input.
- Supporting larger stock tables.
- Improving report formatting.

## License
This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Contact
For questions or suggestions, open an issue or contact [arman.bhatia.1407@gmail.com].
