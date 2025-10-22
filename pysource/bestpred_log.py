import os
from datetime import datetime

def log_check_file_exists(log_file_name):
    file_exists = os.path.isfile(log_file_name)
    return file_exists

def log_create_file(log_file_name):
    try:
        with open(log_file_name, 'x') as log_file:
            current_time = datetime.now()
            long_date = current_time.strftime('%d/%m/%Y')
            curr_time = current_time.strftime('%H:%M:%S')
            log_file.write(f'# {log_file_name} created on {long_date} at {curr_time}\n')
        created_file = 0
    except FileExistsError:
        print(f'[bestpred_log]: Unable to create the log file {log_file_name}')
        created_file = 1
    return created_file

def log_message(log_file, log_message):
    current_time = datetime.now()
    curr_date = current_time.strftime('%Y%m%d')
    curr_time = current_time.strftime('%H:%M:%S')
    log_file_name = f'{log_file}.{curr_date}.log'

    # 1. Check to see if file exists
    file_exists = log_check_file_exists(log_file_name)

    # 2. If file exists:
    if file_exists:
        try:
            with open(log_file_name, 'a') as log_file:
                # 2b. Write the event
                log_file.write(f'[{curr_time}]: {log_message}\n')
        except IOError as e:
            print(f'[bestpred_log]: Error opening file {log_file_name} for writing: {e}')
    # 3. If the file does not exist:
    else:
        # 3a. Create the file
        created_file = log_create_file(log_file_name)
        if created_file == 0:
            try:
                with open(log_file_name, 'a') as log_file:
                    # 3c. Write the event
                    log_file.write(f'[{curr_time}]: {log_message}\n')
            except IOError as e:
                print(f'[bestpred_log]: Error opening file {log_file_name} for writing: {e}')
