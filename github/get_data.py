import pandas as pd

raw = pd.read_excel('Questionario PercepcaoSobreComputacão.xlsx', header=1)

original_columns = raw.columns

raw['Year'] = raw.apply(lambda x: x['Dt_Teleform'][:4] , axis=1)

raw['Gender'] = raw.apply(lambda x: 'F' if x['IT_1'] == 1 else 'M', axis=1)


def get_ed_stage(number):
    if not number:
        return None
    if number == 100000: 
        return 'Middle School'
    elif number == 10000:
        return 'High School (10th Grade)'
    elif number == 1000:
        return 'High School (11th Grade)'
    elif number == 100:
        return 'High School (12th Grade)'
    elif number == 10:
        return 'Adult Education Program'
    elif number == 1:
        return 'College'
    else:
        return None


raw['Educational.Stage'] = raw.apply(lambda x : get_ed_stage(x['ITSDC_03']), axis=1)


def get_fld_interest(number):
    if not number:
        return None
    if number == 100:
        return 'Exact Sciences'
    elif number == 10:
        return 'Biology-Health Sciences'
    elif number == 1:
        return 'Human Sciences'
    else:
        return None
    
        
raw['Field.Of.Interest'] = raw.apply(lambda x : get_fld_interest(x['IT_4']), axis=1)


def get_enroll_cs(number):
    if not number:
        return None
    if number == 100:
        return 'Yes'
    elif number == 10:
        return 'No'
    elif number == 1:
        return 'Maybe'
    else:
        return None


raw['Would.Enroll.In.CS'] = raw.apply(lambda x : get_enroll_cs(x['IT_5']), axis=1)

locations = { 0: 'Uses.Computer.At.Home',
              1: 'Uses.Computer.At.Relatives.House',
              2: 'Uses.Computer.At.Friends.House',
              3: 'Uses.Computer.At.School',
              4: 'Uses.Computer.At.Work',
              5: 'Uses.Computer.At.Lan.House',
              6: 'Uses.Computer.At.Library',
              7: 'Uses.Computer.At.Digital.Inclusion.Center' }

uses = { 0: 'Has.Used.Text.Editor',
         1: 'Has.Used.Image.Editor',
         2: 'Has.Used.Spreadsheet',
         3: 'Has.Used.Database',
         4: 'Has.Used.Internet',
         5: 'Has.Used.Social.Network',
         6: 'Has.Used.Email',
         7: 'Has.Used.Games',
         8: 'Has.Used.For.Creating.Web.Pages',
         9: 'Has.Used.For.Development',
         10:'Has.Used.Other.Softwares' }

for k in locations:
    raw[locations[k]] = None

for k in uses:
    raw[uses[k]] = None

for i, r in raw.iterrows():
    s = str(r['ITSDC_01']).zfill(len(locations))
    for j in range(len(s)):
        if s[j] == '1':
            raw.at[i, locations[j]] = 'Yes'
        else:
            raw.at[i, locations[j]] = 'No'

    s = str(r['ITSDC_02']).zfill(len(uses))
    for j in range(len(s)):
        if s[j] == '1':
            raw.at[i, uses[j]] = 'Yes'
        else:
            raw.at[i, uses[j]] = 'No'    


def get_general(text):
    if not text:
        return None
    if text == 'S':
        return 'Yes'
    elif text == 'N':
        return 'No'
    elif text == 'T':
        return 'Maybe'
    else:
        return None

other = {'IT_8': 'CS.Only.Teaches.To.Use.Software',
         'IT_9': 'CS.Uses.Little.Math',
         'IT_10': 'Most.CS.Students.Are.Male',
         'IT_11': 'CS.Requires.Knowledge.In.Computers',
         'IT_12': 'Higher.Education.Required.To.Work.In.CS',
         'IT_13': 'Family.Approves.CS.Major',
         'IT_14': 'CS.Has.Low.Employability',
         'IT_15': 'CS.Work.Has.Long.Hours',
         'IT_16': 'CS.Fosters.Creativity',
         'IT_17': 'CS.Is.Prestigious',
         'IT_18': 'CS.Provides.Good.Wages',
         'IT_19': 'CS.Enables.Interdisciplinary.Experiences' }

for k in other:
    raw[other[k]] = raw.apply(lambda x: get_general(x[k]), axis=1)

raw = raw[[c for c in raw.columns if c not in original_columns]]
print('raw:', len(raw))

df = pd.read_csv('Data.Survey.csv')
print('df:', len(df))

df = df.append(raw, sort=False)
print('new:', len(df))

df.to_csv('new.csv', index=False)
