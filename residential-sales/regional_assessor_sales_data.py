# This script summarizes real estate transction from Assessor Records
# Summary data is calculated by type and median sales price
# Created by Puget Sound Regional Council Staff
# October 2018

import pandas as pd
import os

start_year = 2016
end_year = 2021

working_directory = os.getcwd()
output_directory = os.path.join(working_directory, 'outputs')

# Create the output directory for the trip generation results
if not os.path.exists(output_directory):
    os.makedirs(output_directory)

##########################################################################################
##########################################################################################
### King County Data    
##########################################################################################
##########################################################################################
print('Working on King County')
input_directory = os.path.join(working_directory, 'data','king')

sales = 'RPSale'
lu_lookup = 'LookUp'
building = 'ResBldg'
condo_units = 'CondoUnit2'
condo_complex = 'CondoComplex'

print('Reading the Extract Files into Memory')
df_sales = pd.read_csv(os.path.join(input_directory, 'EXTR_'+sales+'.csv'),encoding = "ISO-8859-1", low_memory=False, dtype={'Major': str, 'Minor': str})
df_lookup = pd.read_csv(os.path.join(input_directory, 'EXTR_'+lu_lookup+'.csv'), low_memory=False)
df_building = pd.read_csv(os.path.join(input_directory, 'EXTR_'+building+'.csv'), low_memory=False, dtype={'Major': str, 'Minor': str})
df_condo_units = pd.read_csv(os.path.join(input_directory, 'EXTR_'+condo_units+'.csv'), low_memory=False, dtype={'Major': str, 'Minor': str})
df_condo_complex = pd.read_csv(os.path.join(input_directory, 'EXTR_'+condo_complex+'.csv'), low_memory=False, dtype={'Major': str, 'Minor': str})

print('Removing records that do not have a major, minor or sales price')
df_sales.columns = df_sales.columns.str.lower()
df_sales = df_sales[(df_sales.major != 0) & (df_sales.minor != 0) & (df_sales.saleprice != 0)] 
df_sales = df_sales[df_sales.propertytype != 0] 
df_sales['major'] = df_sales['major'].astype(float)
df_sales['minor'] = df_sales['minor'].astype(float)
df_sales['pin'] = df_sales['major']+df_sales['minor']

# Create Trim Down by Principal Use to Condo (2), Apartment (4), Residential (6) or Mobile Home (8)
print('Trimming Sales data to only inlcude Condo, Apartments and Residential Units')
df_sales = df_sales[(df_sales.principaluse == 2) | (df_sales.principaluse == 4) | (df_sales.principaluse == 6) | (df_sales.principaluse == 8)]

print('Calculating Sales year and trimming data between ' + str(start_year) + ' and '+ str(end_year))
df_sales[['month', 'day','year']] = df_sales['documentdate'].str.split('/', expand=True)
df_sales['year'] = df_sales['year'].astype(int)
df_sales['month'] = df_sales['month'].astype(int)
df_sales = df_sales[(df_sales.year >= start_year) & (df_sales.year <= end_year)] 

print('Creating Unique Parcel Sale list - Duplicated sale prices on exact date by same grantee and housing type are combined into 1 parcel')
df_sales['documentdate'] = df_sales['documentdate'].str.replace("/", '_')
df_sales['buyername'] = df_sales['buyername'].str.replace(" ", '_')
df_sales['sales_id'] = df_sales['documentdate'].astype(str) + '_' + df_sales['saleprice'].astype(str) + '_' + df_sales['buyername'].astype(str)

print('Creating Unique Parcel Sale list - Duplicated sale prices on exact date by same grantee and housing type are combined into 1 parcel')
df_unique_parcel = df_sales.groupby(['sales_id']).count()
df_unique_parcel = df_unique_parcel.reset_index()
columns_to_keep = ['sales_id','year']
df_unique_parcel = df_unique_parcel[columns_to_keep]
df_unique_parcel.rename(columns={'year': 'duplicate_sales'}, inplace=True)
df_sales = pd.merge(df_sales, df_unique_parcel, on='sales_id',suffixes=('_x','_y'),how='left')

print('Remove any duplicate transactions - every sale is a single sale to a single buyer')
df_sales = df_sales[(df_sales.duplicate_sales == 1)]

print('Trimming columns in sales records for analysis')
columns_to_keep = ['pin','major','minor','documentdate','saleprice','propertytype','principaluse','year','month']
df_sales = df_sales[columns_to_keep]

print('Cleaning up the Residential Building dataframe to assign Living Units, Size and Zipcode to the sales database')
df_building.columns = df_building.columns.str.lower()
columns_to_keep = ['major','minor','nbrlivingunits','sqfttotliving','zipcode']
df_building = df_building[columns_to_keep]

# Remove records with 0 values on the columns being preserved
df_building = df_building[(df_building.major != 0)]
df_building = df_building[(df_building.minor != 0)]
df_building = df_building[(df_building.nbrlivingunits != 0)]
df_building = df_building[(df_building.zipcode != 0)]
df_building = df_building[(df_building.sqfttotliving != 0)]

# Create a unique pin and then groupby to get a consolidated df of unit counts by unique pin
df_building['major'] = df_building['major'].astype(float)
df_building['minor'] = df_building['minor'].astype(float)
df_building['pin'] = df_building['major']+df_building['minor']

working_columns = ['nbrlivingunits','sqfttotliving','zipcode']

for current_column in working_columns:
    working_df = df_building.groupby(['pin',current_column]).count()
    working_df = working_df.reset_index()
    columns_to_keep = ['pin',current_column]
    working_df = working_df[columns_to_keep]
    df_working_pin = working_df.groupby('pin').max()
    df_working_pin = df_working_pin.reset_index()
    print( 'Joining the ' + current_column + ' to the Sales Dataframe')
    df_sales = pd.merge(df_sales, df_working_pin, on='pin',suffixes=('_x','_y'),how='left')

# Clean up columns in the sales data
df_sales.rename(columns={'nbrlivingunits': 'units'}, inplace=True)
df_sales.rename(columns={'sqfttotliving': 'sqft'}, inplace=True)
df_sales.rename(columns={'saleprice': 'sale_price'}, inplace=True)
df_sales.fillna(0,inplace=True)
df_sales = df_sales[df_sales.propertytype <= 15]

print('Cleaning up the Condo Unit dataframe to assign condos to types with pins in sales database')
df_condo_units.columns = df_condo_units.columns.str.lower()
columns_to_keep = ['major','minor','unittype']
df_condo_units = df_condo_units[columns_to_keep]

# Remove records with 0 values on the columns being preserved
df_condo_units = df_condo_units[(df_condo_units.major != 0)]
df_condo_units = df_condo_units[(df_condo_units.minor != 0)]
df_condo_units = df_condo_units[(df_condo_units.unittype != 0)]

# Remove any Unit Types in the exclusion list - only want residential units, not parking and other items
df_condo_units = df_condo_units[df_condo_units.unittype < 5]
df_condo_units['major'] = df_condo_units['major'].astype(float)
df_condo_units['minor'] = df_condo_units['minor'].astype(float)
df_condo_units['pin'] = df_condo_units['major']+df_building['minor']
df_condos = df_condo_units.groupby(['pin','unittype']).count()
df_condos = df_condos.reset_index()
columns_to_keep = ['pin','unittype']
df_condos = df_condos[columns_to_keep]
df_condos.rename(columns={'unittype': 'condo_type'}, inplace=True)
df_condos_pin = df_condos.groupby('pin').max()
df_condos_pin = df_condos_pin.reset_index()

# Join the Condo Pins with the Sales Database and replace NaN with 0
df_sales = pd.merge(df_sales, df_condos_pin, on='pin',suffixes=('_x','_y'),how='left')
df_sales.fillna(0,inplace=True)

print('Assigning Construction Type to Condo Units')
df_condo_complex.columns = df_condo_complex.columns.str.lower()
columns_to_keep = ['major','constrclass']
df_condo_complex = df_condo_complex[columns_to_keep]
df_condo_complex['high_density'] = 0
df_condo_complex.loc[df_condo_complex['constrclass'] <= 2, 'high_density'] = 1
df_condo_complex = df_condo_complex.drop(['constrclass'],axis=1)
df_condo_complex['major'] = df_condo_complex['major'].astype(float)

# Merge in definition of high density units from the complex definition
df_sales = pd.merge(df_sales, df_condo_complex, on='major',suffixes=('_x','_y'),how='left')
df_sales.fillna(0,inplace=True)

print('Defining Housing Type in Sales data')
df_sales['housing_type'] = 0
df_sales.loc[df_sales['principaluse'] == 8 , 'housing_type'] = 1
df_sales.loc[df_sales['principaluse'] < 6 , 'housing_type'] = 4
df_sales.loc[(df_sales['principaluse'] == 6 ) & (df_sales['condo_type'] > 0 ), 'housing_type'] = 4
df_sales.loc[(df_sales['condo_type'] == 2 ) | (df_sales['condo_type'] == 4 ), 'housing_type'] = 4
df_sales.loc[(df_sales['condo_type'] == 1 ) | (df_sales['condo_type'] == 3 ), 'housing_type'] = 5
df_sales.loc[df_sales['high_density'] == 1, 'housing_type'] = 6
df_sales.loc[df_sales['housing_type'] == 0, 'housing_type'] = 2

print('Defining Housing Sales Price Bin')
df_sales['price_bin'] = 0
starting_price = 0

for current_bin in range (1,51):
    df_sales.loc[(df_sales['sale_price'] >= starting_price) & (df_sales['sale_price'] < starting_price + 50000 ), 'price_bin'] = current_bin
    starting_price = starting_price + 50000

df_sales['county'] = 'king'
df_sales.rename(columns={'pin': 'parcel_number'}, inplace=True)
final_columns = ['parcel_number','month','year','sale_price','price_bin','housing_type','county']
df_regional_sales = df_sales[final_columns]

##########################################################################################
##########################################################################################
### Kitsap County Data    
##########################################################################################
##########################################################################################
print('Working on Kitsap County')
input_directory = os.path.join(working_directory, 'data','kitsap')

print('Reading the Codes into Memory and cleaning columns')
df_codes = pd.read_csv(os.path.join(input_directory, 'Codes.txt'),sep = '\t', encoding='UTF-16')
df_codes = df_codes.reset_index()
df_codes = df_codes.drop(columns=['Description'])
updated_column_names = ['field_name','code','description']
df_codes.columns = updated_column_names

print('Reading the Sales Files into Memory and cleaning columns')

print('Reading in 2015 to 2019 Sales Data')
df_sales_2019 = pd.read_csv(os.path.join(input_directory, 'Residential_Sales_2015-2019.csv'),sep = '\t')
df_sales_2019.columns = df_sales_2019.columns.str.lower()
columns_to_keep = ['acct_no','sale_dt','price']
df_sales_2019 = df_sales_2019[columns_to_keep]
df_sales_2019.rename(columns={'acct_no': 'parcel_number','price':'sale_price'}, inplace=True)

df_sales_2019[['month', 'day','year']] = df_sales_2019['sale_dt'].str.split('/', expand=True)
df_sales_2019['year'] = df_sales_2019['year'].astype(int)
df_sales_2019['month'] = df_sales_2019['month'].astype(int)
df_sales_2019 = df_sales_2019.drop(columns=['day'])
df_sales_2019 = df_sales_2019[(df_sales_2019.year <2017)] 

print('Reading in 2017 to 2022 Sales Data')
df_sales_2022 = pd.read_csv(os.path.join(input_directory, 'Residential_Sales_2017-2022.csv'),sep = ',')
df_sales_2022.columns = df_sales_2022.columns.str.lower()
columns_to_keep = ['tax parcel no.','sale dt','price']
df_sales_2022 = df_sales_2022[columns_to_keep]
df_sales_2022.rename(columns={'tax parcel no.': 'parcel_number','price':'sale_price', 'sale dt': 'sale_dt'}, inplace=True)

df_sales_2022[['month', 'day','year']] = df_sales_2022['sale_dt'].str.split('/', expand=True)
df_sales_2022['year'] = df_sales_2022['year'].astype(int)
df_sales_2022['month'] = df_sales_2022['month'].astype(int)
df_sales_2022 = df_sales_2022.drop(columns=['day'])
df_sales_2022 = df_sales_2022[(df_sales_2022.year >= start_year) & (df_sales_2022.year <= end_year)] 
df_sales_2022['sale_price'] = df_sales_2022['sale_price'].str.replace(',', '')
df_sales_2022['sale_price'] = df_sales_2022['sale_price'].astype(int)

print('Appedning Sales Data to Create One master sale file for Kitsap County')
df_sales = df_sales_2019.append(df_sales_2022)
df_sales = df_sales[(df_sales.year >= start_year) & (df_sales.year <= end_year)] 


print('Reading the Parcel File into Memory and cleaning columns')
df_parcels = pd.read_csv(os.path.join(input_directory, 'Parcels.txt'),sep = '\t')
df_parcels.columns = df_parcels.columns.str.lower()
columns_to_keep = ['acct_no','property_class']
df_parcels = df_parcels[columns_to_keep]
df_parcels.rename(columns={'acct_no': 'parcel_number'}, inplace=True)

print('Merge Parcels and Sales Data to get property type on sales records')
df_sales = pd.merge(df_sales, df_parcels, on='parcel_number',suffixes=('_x','_y'),how='left')
df_sales.fillna(0,inplace=True)

print('Trimming sales data to only inlcude residential units')
df_sales = df_sales[(df_sales.property_class > 0) & (df_sales.property_class <= 150)] 

print('Defining housing type based on prop_class')
df_sales['housing_type'] = 0
df_sales.loc[(df_sales['property_class'] == 118) | (df_sales['property_class'] == 119) | (df_sales['property_class'] == 150 ), 'housing_type'] = 1
df_sales.loc[(df_sales['property_class'] == 111), 'housing_type'] = 2
df_sales.loc[(df_sales['property_class'] >= 131) & (df_sales['property_class'] <= 138 ), 'housing_type'] = 3
df_sales.loc[(df_sales['property_class'] >= 121) & (df_sales['property_class'] <= 123 ), 'housing_type'] = 4
df_sales.loc[(df_sales['property_class'] == 141), 'housing_type'] = 5

print('Defining sales bins')
df_sales['price_bin'] = 0
starting_price = 0

for current_bin in range (1,51):
    df_sales.loc[(df_sales['sale_price'] >= starting_price) & (df_sales['sale_price'] < starting_price + 50000 ), 'price_bin'] = current_bin
    starting_price = starting_price + 50000

df_sales['county'] = 'kitsap'
final_columns = ['parcel_number','month','year','sale_price','price_bin','housing_type','county']
df_sales = df_sales[final_columns]

print('Appending Kitsap County Sales data to the Regional Sales Database')
df_regional_sales = df_regional_sales.append(df_sales,ignore_index=True)

##########################################################################################
##########################################################################################
### Pierce County Data    
##########################################################################################
##########################################################################################
print('Working on Pierce County')
input_directory = os.path.join(working_directory, 'data','pierce')

print('Reading the Sales Extract File into Memory and cleaning columns')
df_sales = pd.read_csv(os.path.join(input_directory, 'sale.txt'),header=None,sep = '|', encoding = "ISO-8859-1", low_memory=False)
column_names = ['etn','parcel_count','parcel_number','sale_date','sale_price','deed_type','grantor','grantee','valid','confirmed','exclude','improved', 'appraisal_account']
df_sales.columns = column_names
columns_to_keep = ['parcel_number','sale_date','sale_price','grantee']
df_sales = df_sales[columns_to_keep]

print('Calculating Sales year and trimming data between ' + str(start_year) + ' and '+ str(end_year))
df_sales[['month', 'day','year']] = df_sales['sale_date'].str.split('/', expand=True)
df_sales['month'] = df_sales['month'].astype(int)
df_sales['year'] = df_sales['year'].astype(int)
df_sales = df_sales[(df_sales.year >= start_year) & (df_sales.year <= end_year)] 
columns_to_keep = ['parcel_number','sale_date','sale_price','grantee','month','year']
df_sales = df_sales[columns_to_keep]

print('Reading the Improvement Extract File into Memory and cleaning columns')
df_improvements = pd.read_csv(os.path.join(input_directory, 'improvement.txt'),header=None,sep = '|', low_memory=False)
column_names = ['parcel_number','building_id','property_type','neighborhood','neighborhood_ext','sqft',
                'net_sqft','percent_complete','condition','quality','occupancy_code','occupancy_description',
                'mobile_home_number','mobile_home_length','mobile_home_make','attic_sf','basement_sf','finished_basement',
                'carport_sf','balcony_sf','porch_sf','attached_garage','detached_garage','fireplaces','basement_garage_door']

df_improvements.columns = column_names
columns_to_keep = ['parcel_number','building_id','property_type','sqft','occupancy_description']
df_improvements = df_improvements[columns_to_keep]

print('Calculating Housing Type from Occupancy Description')
exclude_types = ['Industrial','Out Building']

for buildings in exclude_types:
    df_improvements = df_improvements[(df_improvements.property_type != buildings)] 

apartment_codes = ['Apartment Conv w/4-8 Units','Apartment High Rise','Apartment w/4-8 Units',
                     'Apartments (Hi-Rise)','Apt Low Rise 100 Units Plus','Apt Low Rise 20 to 99 Units',
                     'Apt Low Rise up to 19 Units','Apts w/4-19 Units']
low_end_condo_codes = ['Condo - Separate Unit','Condo Apartment Low Rise','Condo Low Rise']
high_end_condo_codes = ['Condo Apartment High Rise','Condo High Rise']
single_family_codes = ['Single Family Residential','Single Family Residential ']
townhome_codes = ['Duplex','Duplex Conv','Townhouse','Townhouse/Condo','Triplex','Triplex Conv']

df_improvements['housing_type'] = 0

df_improvements.loc[df_improvements['occupancy_description'] == 'Mobile or Manufactured Home', 'housing_type'] = 1

for codes in single_family_codes:
    df_improvements.loc[df_improvements['occupancy_description'] == codes, 'housing_type'] = 2  

for codes in apartment_codes:
    df_improvements.loc[df_improvements['occupancy_description'] == codes, 'housing_type'] = 3

for codes in townhome_codes:
    df_improvements.loc[df_improvements['occupancy_description'] == codes, 'housing_type'] = 4 

for codes in low_end_condo_codes:
    df_improvements.loc[df_improvements['occupancy_description'] == codes, 'housing_type'] = 5

for codes in high_end_condo_codes:
    df_improvements.loc[df_improvements['occupancy_description'] == codes, 'housing_type'] = 6

print('Removing Improvements with a Housing Type of 0')
df_improvements = df_improvements[(df_improvements.housing_type != 0)]

print('Removing duplicate parcels in improvement database to merge housing type with sales data')
df_housing = df_improvements.groupby(['parcel_number']).max()
df_housing = df_housing.reset_index()
columns_to_keep = ['parcel_number','housing_type']
df_housing = df_housing[columns_to_keep]

print('Merging the housing type with the sale records to identify housing type in sales')
df_sales = pd.merge(df_sales, df_housing, on='parcel_number',suffixes=('_x','_y'),how='left')
df_sales.fillna(0,inplace=True)
df_sales = df_sales[(df_sales.housing_type != 0)]

print('Creating Unique Parcel Sale list - Duplicated sale prices on exact date by same grantee and housing type are combined into 1 parcel')
df_sales['sale_date'] = df_sales['sale_date'].str.replace("/", '_')
df_sales['grantee'] = df_sales['grantee'].str.replace(" ", '_')
df_sales['sales_id'] = df_sales['sale_date'].astype(str) + '_' + df_sales['sale_price'].astype(str) + '_' + df_sales['grantee'].astype(str)

print('Creating Unique Parcel Sale list - Duplicated sale prices on exact date by same grantee and housing type are combined into 1 parcel')
df_unique_parcel = df_sales.groupby(['sales_id']).count()
df_unique_parcel = df_unique_parcel.reset_index()
columns_to_keep = ['sales_id','year']
df_unique_parcel = df_unique_parcel[columns_to_keep]
df_unique_parcel.rename(columns={'year': 'duplicate_sales'}, inplace=True)
df_sales = pd.merge(df_sales, df_unique_parcel, on='sales_id',suffixes=('_x','_y'),how='left')

print('Remove any duplicate transactions - every sale is a single sale to a single buyer')
df_sales = df_sales[(df_sales.duplicate_sales == 1)]

print('Adding a sales price bin to the sales record')
df_sales['price_bin'] = 0
starting_price = 0

for current_bin in range (1,51):
    df_sales.loc[(df_sales['sale_price'] >= starting_price) & (df_sales['sale_price'] < starting_price + 50000 ), 'price_bin'] = current_bin
    starting_price = starting_price + 50000

df_sales['county'] = 'pierce'

print('Creating Final Sales Record database to be used for regional totals')
final_columns = ['parcel_number','month','year','sale_price','price_bin','housing_type','county']
df_sales = df_sales[final_columns]

print('Appending Pierce County Sales data to the Regional Sales Database')
df_regional_sales = df_regional_sales.append(df_sales,ignore_index=True)

##########################################################################################
##########################################################################################
### Snohomish County Data    
##########################################################################################
##########################################################################################
print('Working on Snohomish County')
input_directory = os.path.join(working_directory, 'data','snohomish')

print('Reading the Extract File into Memory')
df_sales = pd.read_csv(os.path.join(input_directory, 'AllSales_20211006.csv'), low_memory=False)

print('Removing unnecceasry columns from sales data')
df_sales.columns = df_sales.columns.str.lower()
columns_to_keep = ['parcel_id','prop_class','propertystreet','sale_date','sale_price','ownername1']
df_sales = df_sales[columns_to_keep]
df_sales  = df_sales.rename(columns={'parcel_id':'parcel_number'})

print('Trimming data to only include residential transactions')
df_sales = df_sales[(df_sales.prop_class >= 110) & (df_sales.prop_class <= 145)] 

print('Removing records that do not have a sales date, price or address')
df_sales.fillna(0,inplace=True)
df_sales = df_sales[(df_sales.sale_date != 0) & (df_sales.sale_price != 0) & (df_sales.propertystreet != 0) ] 

print('Calculating Sales year and trimming data between ' + str(start_year) + ' and '+ str(end_year))
df_sales[['month', 'day','year']] = df_sales['sale_date'].str.split('-', expand=True)
df_sales['month'] = df_sales['month'].astype(int)
df_sales['year'] = df_sales['year'].astype(int) + 2000
df_sales = df_sales[(df_sales.year >= start_year) & (df_sales.year <= end_year)] 

print('Defining Housing Sales Price Bin')
df_sales['sale_price'] = df_sales['sale_price'].str.replace(',', '')
df_sales['sale_price'] = df_sales['sale_price'].astype(int)

print('Defining housing type based on prop_class')
df_sales['housing_type'] = 0
df_sales.loc[(df_sales['prop_class'] >= 117) & (df_sales['prop_class'] <= 119 ), 'housing_type'] = 1
df_sales.loc[(df_sales['prop_class'] >= 110) & (df_sales['prop_class'] <= 115 ), 'housing_type'] = 2
df_sales.loc[(df_sales['prop_class'] >= 130) & (df_sales['prop_class'] <= 139 ), 'housing_type'] = 3
df_sales.loc[(df_sales['prop_class'] == 116) | (df_sales['prop_class'] == 121 ) | (df_sales['prop_class'] == 122 )| (df_sales['prop_class'] == 123 )| (df_sales['prop_class'] == 124 ), 'housing_type'] = 4
df_sales.loc[(df_sales['prop_class'] >= 141) & (df_sales['prop_class'] <= 145 ), 'housing_type'] = 5

print('Creating Unique Parcel Sale list - Duplicated sale prices on exact date by same owner and housing type are combined into 1 parcel')
df_sales['sale_date'] = df_sales['sale_date'].str.replace("/", '_')
df_sales['ownername1'] = df_sales['ownername1'].str.replace(" ", '_')
df_sales['sales_id'] = df_sales['sale_date'].astype(str) + '_' + df_sales['sale_price'].astype(str) + '_' + df_sales['ownername1'].astype(str)

print('Creating Unique Parcel Sale list - Duplicated sale prices on exact date by same grantee and housing type are combined into 1 parcel')
df_unique_parcel = df_sales.groupby(['sales_id']).count()
df_unique_parcel = df_unique_parcel.reset_index()
columns_to_keep = ['sales_id','year']
df_unique_parcel = df_unique_parcel[columns_to_keep]
df_unique_parcel.rename(columns={'year': 'duplicate_sales'}, inplace=True)
df_sales = pd.merge(df_sales, df_unique_parcel, on='sales_id',suffixes=('_x','_y'),how='left')

print('Remove any duplicate transactions - every sale is a single sale to a single buyer')
df_sales = df_sales[(df_sales.duplicate_sales == 1)]

print('Defining sales bins')
df_sales['price_bin'] = 0
starting_price = 0

for current_bin in range (1,51):
    df_sales.loc[(df_sales['sale_price'] >= starting_price) & (df_sales['sale_price'] < starting_price + 50000 ), 'price_bin'] = current_bin
    starting_price = starting_price + 50000

df_sales['county'] = 'snohomish'
final_columns = ['parcel_number','month','year','sale_price','price_bin','housing_type','county']
df_sales = df_sales[final_columns]

print('Appending Snohomish County Sales data to the Regional Sales Database')
df_regional_sales = df_regional_sales.append(df_sales,ignore_index=True)

#####################################################################################################
#####################################################################################################
### Summarize Transactions
#####################################################################################################
#####################################################################################################
df_regional_sales.to_csv(os.path.join(output_directory,'total_residential_sales_transactions_region.csv'),index=False)

print('Calculating total transactions by year, county and housing type')
df_summary = df_regional_sales.groupby(['year','housing_type','county']).count()
df_summary = df_summary.reset_index()
columns_to_keep = ['year','housing_type','county','sale_price']
df_summary = df_summary[columns_to_keep]
df_summary.rename(columns={'sale_price': 'transactions'}, inplace=True)
df_summary.to_csv(os.path.join(output_directory,'summarized_transactions_by_year_type_region.csv'),index=False)

print('Calculating total transactions by year, housing type and price bin')
df_summary = df_regional_sales.groupby(['year','housing_type','price_bin','county']).count()
df_summary = df_summary.reset_index()
columns_to_keep = ['year','housing_type','price_bin','county','sale_price']
df_summary = df_summary[columns_to_keep]
df_summary.rename(columns={'sale_price': 'transactions'}, inplace=True)
df_summary.to_csv(os.path.join(output_directory,'summarized_transactions_by_year_type_price_region.csv'),index=False)

print('Calculating median sales price by property type and year')
columns_to_keep = ['year', 'housing_type', 'county', 'sale_price']
temp = df_regional_sales[columns_to_keep]
df_summary = temp.groupby(['year','housing_type','county']).quantile(0.50) 
df_summary = df_summary.reset_index()
columns_to_keep = ['year','housing_type','county','sale_price']
df_summary = df_summary[columns_to_keep]
df_summary.rename(columns={'sale_price': 'median-sales-price'}, inplace=True)
df_summary.to_csv(os.path.join(output_directory,'median_price_by_type_region.csv'),index=False)
