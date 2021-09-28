import sys, os
from . import getch
import datetime


class DisplayDatabase:
    def __init__(self, tablename=None):

        from . import location_database

        query = location_database.session.query(location_database.database_location)
        results = query.all()
        if len(results) == 0:
            print("No tables registered, sorry...")
            sys.exit(-1)

        all_tablenames = [result.table_name for result in results]

        if not tablename:
            pass
        elif tablename not in all_tablenames:
            print("Unknown table name.")
            tablename = None

        if not tablename:
            print("Please choose one of the following tables:")
            for result in results:
                print(result)
            while True:
                choice = input("ID (q to quit): ")
                if choice in ["q", "Q"]:
                    sys.exit(0)
                try:
                    choice = int(choice)
                    tablename = [
                        result.table_name for result in results if result.id == choice
                    ]
                    if tablename == []:
                        print("ID not valid.")
                    else:
                        tablename = str(tablename[0])
                        break
                except:
                    print("ID needs to be of type integer, please try again...")

        print(tablename)

        table = [table for table in results if tablename == table.table_name]
        table = table[0]

        package = __import__(table.class_in)
        database = getattr(package, "database")
        self.entry_type = getattr(database, tablename)

        # sys.exit(0)
        #
        #        if tablename == "experiments":         # that needs to disappear
        ##            from esm_runscripts import database
        #            self.entry_type = database.experiment
        #        else:
        #            print ("Unknown table, quitting...")
        #            sys.exit(-1)
        #
        self.session = database.session
        query = database.session.query(self.entry_type)
        results = query.all()
        if not type(results) == list:
            results = [results]

        self.query = query
        self.verbose = False
        self.all_results = results
        self.results = results
        table = getattr(self.entry_type, "__table__")
        columns = getattr(table, "columns")
        self.columns = [str(column).replace(tablename + ".", "") for column in columns]

        while True:
            self.output_writer()
            self.decision_maker()

    def output_writer(self):
        lines_of_terminal = os.get_terminal_size().lines

        for line in range(0, lines_of_terminal):
            print()
        if not self.verbose:
            getattr(self.entry_type, "topline")()
        for result in self.results:
            if self.verbose:
                getattr(self.entry_type, "nicer_output")(result)
            else:
                print(result)
        print(
            "------------------------------------------------------------------------------------------------------------------------------"
        )
        print(
            "[S]elect Entry        [V]erbose on/off        [R]eset selection        [E]dit Entry        [D]elete Entry               [Q]uit"
        )
        print(
            "------------------------------------------------------------------------------------------------------------------------------"
        )

    def ask_column(self):
        find = input("Which column " + str(self.columns) + "? ").lower()

        if find == "":
            return None

        for column in self.columns:
            if column.startswith(find):
                find_column = column
                break

        if not find_column:
            print("Unknown column.")
            return None

        return find_column

    def ask_dataset(self):
        edit_id = None
        if len(self.results) == 0:
            return None

        elif len(self.results) == 1:
            print("Select entry id=" + str(self.results[0].id) + " [y/n]?")

            yesno = getch.get_one_of(["y", "Y", "n", "N"])
            if yesno.lower() == "y":
                edit_id = self.results[0].id
                edit_dataset = self.results[0]

        if not edit_id:
            edit_id = input("Please enter dataset id: ")
            if edit_id == "":
                return None
            try:
                edit_id = int(edit_id)
            except:
                print("id needs to be of type int.")
                return None
            edit_dataset = [
                result for result in self.all_results if result.id == edit_id
            ]
            if edit_dataset == []:
                print("Unknown dataset id.")
                return None
            else:
                edit_dataset = edit_dataset[0]

        return edit_dataset

    def decision_maker(self):
        char = getch.get_one_of(["q", "Q", "v", "V", "s", "S", "e", "E", "r", "R", "D"])

        if char.lower() == "q":
            sys.exit(0)

        elif char.lower() == "v":
            self.verbose = not self.verbose

        elif char.lower() == "s":
            self.select_stuff()

        elif char.lower() == "e":
            self.edit_dataset()

        elif char.lower() == "r":
            self.results = self.all_results

        elif char == "D":
            self.remove_datasets()

    def remove_datasets(self):
        all_ids = [result.id for result in self.results]
        print("Really remove entry/entries " + str(all_ids) + " [Y/n]? ")
        really = getch.get_one_of(["Y", "n", "N"])
        if really == "Y":
            for this_id in all_ids:
                self.query.filter_by(id=this_id).delete()
            self.session.commit()

            self.all_results = [
                result for result in self.all_results if result.id not in all_ids
            ]
            self.results = self.all_results

    def edit_dataset(self):
        edit_dataset = self.ask_dataset()
        if not edit_dataset:
            return

        edit_column = self.ask_column()
        if not edit_column:
            return
        column_type = type(getattr(self.all_results[0], edit_column))

        edit_entry = input("Change to? ")
        if edit_entry == "":
            return

        # try to convert user input
        if column_type == int:
            try:
                edit_entry = int(edit_entry)
            except:
                print("Wrong data type, should be int!")
                return
        elif column_type == datetime.datetime:
            try:
                edit_entry = datetime.datetime(edit_entry)
            except:
                print("Wrong data type, should be datetime.datetime!")
                return

        this_entry = self.query.filter_by(id=edit_dataset.id).first()
        try:
            setattr(this_entry, edit_column, edit_entry)
            self.session.commit()
        except:
            print(
                "Problem changing column "
                + edit_column
                + " of dataset "
                + str(edit_dataset.id)
                + ". "
            )
            return

        new_results = []
        new_all_results = []
        for result in self.all_results:
            need_this = False
            if result in self.results:
                need_this = True
            if result == edit_dataset:
                setattr(result, edit_column, edit_entry)
            new_all_results += [result]
            if need_this:
                new_results += [result]
        self.results = new_results
        self.all_results = new_all_results

    def select_stuff(self):
        find_column = self.ask_column()
        if not find_column:
            return

        find_value = input("Search for? ")
        if find_value == "":
            return

        if find_column and find_value:
            if find_column == "id":
                self.results = [
                    result
                    for result in self.results
                    if find_value == str(getattr(result, find_column))
                ]
            else:
                self.results = [
                    result
                    for result in self.results
                    if find_value in str(getattr(result, find_column))
                ]
