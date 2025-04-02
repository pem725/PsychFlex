import pandas as pd
import os
import re
import argparse

def format_stability_table(csv_path, output_format="markdown"):
    """Format the stability analysis table for Google Docs"""
    # Read data if it exists
    if os.path.exists(csv_path):
        df = pd.read_csv(csv_path)
    else:
        # Create sample data if file doesn't exist
        data = {
            "Construct": ["Happiness (SHS)", "Happiness (SHS)", "Happiness (SHS)",
                          "Purpose (BPURP)", "Purpose (BPURP)", "Purpose (BPURP)"],
            "Time_Points": ["Baseline to 6-month", "Baseline to 2-year", "6-month to 2-year",
                            "Baseline to 6-month", "Baseline to 2-year", "6-month to 2-year"],
            "Correlation": [0.654, 0.528, 0.716, 0.602, 0.547, 0.683]
        }
        df = pd.DataFrame(data)
    
    # Format correlation values to 3 decimal places
    df['Correlation'] = df['Correlation'].apply(lambda x: f"{x:.3f}")
    
    # Rename columns for better readability
    df = df.rename(columns={
        "Time_Points": "Time Points",
    })
    
    # Format according to requested output
    if output_format == "markdown":
        return df.to_markdown(index=False)
    elif output_format == "html":
        return df.to_html(index=False)
    elif output_format == "csv":
        return df.to_csv(index=False)
    elif output_format == "tsv":
        return df.to_csv(index=False, sep='\t')
    else:
        return df

def format_correlates_table(csv_path, output_format="markdown"):
    """Format the correlates analysis table for Google Docs"""
    # Read data if it exists
    if os.path.exists(csv_path):
        df = pd.read_csv(csv_path)
    else:
        # Create sample data if file doesn't exist
        data = {
            "Variable": ["Hope", "Self-Control", "Need Satisfaction (Autonomy)", 
                         "Need Satisfaction (Competence)", "Need Satisfaction (Relatedness)",
                         "Positive Affect", "Negative Affect", "Life Satisfaction"],
            "Purpose (r)": [0.572, 0.483, 0.413, 0.501, 0.376, 0.422, -0.316, 0.537],
            "Happiness (r)": [0.612, 0.378, 0.461, 0.513, 0.492, 0.687, -0.518, 0.721],
            "Difference": [-0.040, 0.105, -0.048, -0.012, -0.116, -0.265, 0.202, -0.184]
        }
        df = pd.DataFrame(data)
    
    # Format numeric values to 3 decimal places
    for col in ["Purpose (r)", "Happiness (r)", "Difference"]:
        if col in df.columns:
            df[col] = df[col].apply(lambda x: f"{x:.3f}")
    
    # Format according to requested output
    if output_format == "markdown":
        return df.to_markdown(index=False)
    elif output_format == "html":
        return df.to_html(index=False)
    elif output_format == "csv":
        return df.to_csv(index=False)
    elif output_format == "tsv":
        return df.to_csv(index=False, sep='\t')
    else:
        return df

def format_longitudinal_table(csv_path, timepoint, output_format="markdown"):
    """Format the longitudinal analysis table for Google Docs"""
    # Read data if it exists
    if os.path.exists(csv_path):
        df = pd.read_csv(csv_path)
    else:
        # Create sample data if file doesn't exist
        data = {
            "Outcome": ["Life Satisfaction", "Depression", "Positive Affect"],
            "Purpose (r)": [0.412, -0.278, 0.385],
            "Happiness (r)": [0.481, -0.335, 0.464]
        }
        df = pd.DataFrame(data)
    
    # Format numeric values to 3 decimal places
    for col in ["Purpose (r)", "Happiness (r)"]:
        if col in df.columns:
            df[col] = df[col].apply(lambda x: f"{x:.3f}")
    
    # Add title based on timepoint
    title = f"{timepoint.capitalize()} Longitudinal Correlations"
    
    # Format according to requested output
    if output_format == "markdown":
        md = f"# {title}\n\n"
        md += df.to_markdown(index=False)
        return md
    elif output_format == "html":
        html = f"<h3>{title}</h3>\n"
        html += df.to_html(index=False)
        return html
    elif output_format == "csv":
        return df.to_csv(index=False)
    elif output_format == "tsv":
        return df.to_csv(index=False, sep='\t')
    else:
        return df

def process_r_output(output_text):
    """Process raw R output text and convert to structured tables"""
    # Define patterns to identify tables
    stability_pattern = r"=== STABILITY ANALYSIS ===\s+(.+?)(?:\n\n|\Z)"
    correlates_pattern = r"=== CORRELATIONAL ANALYSIS ===\s+(.+?)(?:\n\n|\Z)" 
    longitudinal_6mo_pattern = r"=== LONGITUDINAL ANALYSIS \(6-month\) ===\s+(.+?)(?:\n\n|\Z)"
    longitudinal_2yr_pattern = r"=== LONGITUDINAL ANALYSIS \(2-year\) ===\s+(.+?)(?:\n\n|\Z)"
    
    tables = {}
    
    # Extract text for each table section
    stability_match = re.search(stability_pattern, output_text, re.DOTALL)
    if stability_match:
        tables['stability'] = stability_match.group(1)
    
    correlates_match = re.search(correlates_pattern, output_text, re.DOTALL)
    if correlates_match:
        tables['correlates'] = correlates_match.group(1)
    
    longitudinal_6mo_match = re.search(longitudinal_6mo_pattern, output_text, re.DOTALL)
    if longitudinal_6mo_match:
        tables['longitudinal_6mo'] = longitudinal_6mo_match.group(1)
    
    longitudinal_2yr_match = re.search(longitudinal_2yr_pattern, output_text, re.DOTALL)
    if longitudinal_2yr_match:
        tables['longitudinal_2yr'] = longitudinal_2yr_match.group(1)
    
    return tables

def parse_r_table(table_text):
    """Parse R console output table into a pandas DataFrame"""
    lines = table_text.strip().split('\n')
    # Find header line
    header_idx = 0
    for i, line in enumerate(lines):
        if not line.startswith(' ') and not line.startswith('#'):
            header_idx = i
            break
    
    header = lines[header_idx].split()
    data = []
    
    for line in lines[header_idx+1:]:
        if line.strip():
            # Split by whitespace but preserve quoted strings
            row = []
            in_quotes = False
            current_field = ""
            
            for char in line:
                if char == '"' and not in_quotes:
                    in_quotes = True
                elif char == '"' and in_quotes:
                    in_quotes = False
                elif char.isspace() and not in_quotes:
                    if current_field:
                        row.append(current_field)
                        current_field = ""
                else:
                    current_field += char
            
            if current_field:
                row.append(current_field)
            
            # Ensure row has same length as header
            while len(row) < len(header):
                row.append("")
            data.append(row[:len(header)])
    
    return pd.DataFrame(data, columns=header)

def main():
    """Command line interface for table formatting"""
    parser = argparse.ArgumentParser(description='Format R analysis tables for Google Docs')
    parser.add_argument('--input', '-i', help='R output file path or directory containing CSV tables')
    parser.add_argument('--output', '-o', default='tables', help='Output directory for formatted tables')
    parser.add_argument('--format', '-f', default='markdown', choices=['markdown', 'html', 'csv', 'tsv'],
                       help='Output format for tables')
    
    args = parser.parse_args()
    
    if args.input and os.path.isfile(args.input):
        convert_r_output_to_gdocs_tables(args.input, args.output, args.format)
    else:
        print("No input file specified. Creating example tables...")
        # Create sample tables
        os.makedirs(args.output, exist_ok=True)
        
        # Stability table
        stability_table = format_stability_table(None, args.format)
        with open(os.path.join(args.output, f"table1_stability.{get_file_ext(args.format)}"), 'w') as f:
            f.write(stability_table)
            
        # Correlates table
        correlates_table = format_correlates_table(None, args.format)
        with open(os.path.join(args.output, f"table2_correlates.{get_file_ext(args.format)}"), 'w') as f:
            f.write(correlates_table)
            
        # 6-month Longitudinal table
        longitudinal_6mo_table = format_longitudinal_table(None, "Six-month", args.format)
        with open(os.path.join(args.output, f"table3_longitudinal_6mo.{get_file_ext(args.format)}"), 'w') as f:
            f.write(longitudinal_6mo_table)
            
        # 2-year Longitudinal table
        longitudinal_2yr_table = format_longitudinal_table(None, "Two-year", args.format)
        with open(os.path.join(args.output, f"table4_longitudinal_2yr.{get_file_ext(args.format)}"), 'w') as f:
            f.write(longitudinal_2yr_table)
            
        print(f"Example tables saved to {args.output} directory in {args.format} format")

def get_file_ext(format_type):
    """Get file extension based on format type"""
    if format_type == "markdown":
        return "md"
    elif format_type == "html":
        return "html"
    elif format_type == "csv":
        return "csv"
    elif format_type == "tsv":
        return "txt"
    return "txt"

def convert_r_output_to_gdocs_tables(r_output_file, output_dir="tables", format="markdown"):
    """Convert R console output to Google Docs-friendly tables"""
    with open(r_output_file, 'r') as f:
        r_output = f.read()
    
    # Process the R output
    tables_text = process_r_output(r_output)
    
    # Create output directory if it doesn't exist
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
    
    # Process each table
    formatted_tables = {}
    
    # Stability table
    if 'stability' in tables_text:
        df = parse_r_table(tables_text['stability'])
        
        if format == "markdown":
            table_content = df.to_markdown(index=False)
            filename = "table1_stability.md"
        elif format == "html":
            table_content = df.to_html(index=False)
            filename = "table1_stability.html"
        elif format == "csv":
            table_content = df.to_csv(index=False)
            filename = "table1_stability.csv"
        elif format == "tsv":
            table_content = df.to_csv(index=False, sep='\t')
            filename = "table2_correlates.txt"
            
        with open(os.path.join(output_dir, filename), 'w') as f:
            f.write(table_content)
            
        formatted_tables['correlates'] = {
            'content': table_content,
            'file': os.path.join(output_dir, filename)
        }
    
    # 6-month Longitudinal table
    if 'longitudinal_6mo' in tables_text:
        df = parse_r_table(tables_text['longitudinal_6mo'])
        
        if format == "markdown":
            table_content = df.to_markdown(index=False)
            filename = "table3_longitudinal_6mo.md"
        elif format == "html":
            table_content = df.to_html(index=False)
            filename = "table3_longitudinal_6mo.html"
        elif format == "csv":
            table_content = df.to_csv(index=False)
            filename = "table3_longitudinal_6mo.csv"
        elif format == "tsv":
            table_content = df.to_csv(index=False, sep='\t')
            filename = "table3_longitudinal_6mo.txt"
            
        with open(os.path.join(output_dir, filename), 'w') as f:
            f.write(table_content)
            
        formatted_tables['longitudinal_6mo'] = {
            'content': table_content,
            'file': os.path.join(output_dir, filename)
        }
    
    # 2-year Longitudinal table
    if 'longitudinal_2yr' in tables_text:
        df = parse_r_table(tables_text['longitudinal_2yr'])
        
        if format == "markdown":
            table_content = df.to_markdown(index=False)
            filename = "table4_longitudinal_2yr.md"
        elif format == "html":
            table_content = df.to_html(index=False)
            filename = "table4_longitudinal_2yr.html"
        elif format == "csv":
            table_content = df.to_csv(index=False)
            filename = "table4_longitudinal_2yr.csv"
        elif format == "tsv":
            table_content = df.to_csv(index=False, sep='\t')
            filename = "table4_longitudinal_2yr.txt"
            
        with open(os.path.join(output_dir, filename), 'w') as f:
            f.write(table_content)
            
        formatted_tables['longitudinal_2yr'] = {
            'content': table_content,
            'file': os.path.join(output_dir, filename)
        }
    
    print(f"Tables saved to {output_dir} directory in {format} format")
    return formatted_tables

if __name__ == "__main__":
    main()_content = df.to_csv(index=False, sep='\t')
            filename = "table1_stability.txt"
            
        with open(os.path.join(output_dir, filename), 'w') as f:
            f.write(table_content)
            
        formatted_tables['stability'] = {
            'content': table_content,
            'file': os.path.join(output_dir, filename)
        }
    
    # Correlates table
    if 'correlates' in tables_text:
        df = parse_r_table(tables_text['correlates'])
        
        if format == "markdown":
            table_content = df.to_markdown(index=False)
            filename = "table2_correlates.md"
        elif format == "html":
            table_content = df.to_html(index=False)
            filename = "table2_correlates.html"
        elif format == "csv":
            table_content = df.to_csv(index=False)
            filename = "table2_correlates.csv"
        elif format == "tsv":
            table