from . import database
from datetime import datetime


def database_entry(action, setup_name, base_dir):

    print (action)
    print (setup_name)
    print (base_dir)

    thisrun = database.session.query(database.installation).filter_by(setup_name = setup_name).filter_by(folder = base_dir + "/" + setup_name).all()

    for run in thisrun:
        print (run) 

    if thisrun == []:
        thisrun = database.installation(
                action = action,
                setup_name = setup_name,
                timestamp = datetime.now(),
                folder = base_dir + "/" + setup_name
                )
        database.session.add(thisrun)
    else:
        thisrun = thisrun[-1]
        thisrun.action = action
        thisrun.timestamp = datetime.now()
    database.session.commit()
    print("beep")
    return database
