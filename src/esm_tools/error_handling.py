import re
import sys

import colorama


def user_note(
    note_heading, note_text, color=colorama.Fore.YELLOW, dsymbols=["``"], hints=[]
):
    """
    Notify the user about something. In the future this should also write in the log.

    Parameters
    ----------
    note_heading : str
        Note type used for the heading.
    text : str
        Text clarifying the note.
    """
    reset_s = colorama.Style.RESET_ALL

    # Convert a list of strings to a single string
    if isinstance(note_text, list):
        new_note_text = ""
        for item in note_text:
            new_note_text = f"{new_note_text}- {item}\n"
        note_text = new_note_text

    # Add hints to the note_text
    note_text = user_note_hints(note_text, hints)

    # Add color to the note_text
    for dsymbol in dsymbols:
        note_text = re.sub(
            f"{dsymbol}([^{dsymbol}]*){dsymbol}", f"{color}\\1{reset_s}", str(note_text)
        )
    print(f"\n{color}{note_heading}\n{'-' * len(note_heading)}{reset_s}")
    print(f"{note_text}\n")


def user_error(error_type, error_text, exit_code=1, dsymbols=["``"], hints=[]):
    """
    User-friendly error using ``sys.exit()`` instead of an ``Exception``.

    Parameters
    ----------
    error_type : str
        Error type used for the error heading.
    text : str
        Text clarifying the error.
    exit_code : int
        The exit code to send back to the parent process (default to 1)
    """
    error_title = "ERROR: " + error_type
    user_note(
        error_title, error_text, color=colorama.Fore.RED, dsymbols=dsymbols, hints=hints
    )
    sys.exit(exit_code)


def user_note_hints(note_text, hints):
    """
    Add hints to the note text. The hints are added to the note text by replacing
    the placeholders "@HINT_#@" with the actual hints from the hints list.

    Parameters
    ----------
    note_text : str
        The note text with placeholders for the hints. The placeholders are in the
        form "@HINT_#@", where # is the index of the hint in the hints list.
    hints : list
        A list of hints to be added to the note text. Each hint is a dictionary with
        the following keys:
        - type: The type of the hint (e.g., "prov" for provenance)
        - text: The text of the hint. This text can contain a placeholder "@HINT@"
          which will be replaced with the actual hint corresponding to its index.
        - object: The object to which the hint applies

    Returns
    -------
    note_text : str
        The note text with the placeholders replaced with the hints.
    """

    # Find all hints matching r"@HINT_(\d+)@" in the note_text
    pattern = r"@HINT_(\d+)@"
    hint_indexes = [int(x) for x in list(set(re.findall(pattern, note_text)))]
    hint_indexes.sort()

    # Check whether all hint indexes are represented in the hints list
    if hint_indexes != list(range(len(hints))):
        raise ValueError(
            "The hint indexes in the note_text do not match the hints list."
        )

    # Loop through the hints and replace the placeholders in the note_text
    for indx, hint in enumerate(hints):
        # Hint type provenance
        if hint["type"] == "prov":
            hint_text = hint["text"]
            mapping_with_provenance = hint["object"]

            # Get the provenance
            provenance = None
            if hasattr(mapping_with_provenance, "extract_first_nested_values_provenance"):
                provenance = mapping_with_provenance.extract_first_nested_values_provenance()
            else:
                logging.debug("No provenance found for %s", mapping_with_provenance)
            # If the provenance is found, replace the placeholder with the provenance,
            # otherwise remove the placeholder (provenance might not always exist)
            if provenance:
                prov_string = (
                    f"``{provenance['yaml_file']}``,"
                    f"line:``{provenance['line']}``,"
                    f"col:``{provenance['col']}``"
                )
                # Replace the HINT placeholder of the hint with the provenance string
                hint_text = hint["text"].replace("@HINT@", prov_string)
                # Replace the HINT placeholder on the note message with the final hint
                note_text = note_text.replace(f"@HINT_{indx}@", hint_text)
            else:
                note_text = note_text.replace(f"@HINT_{indx}@", "")
        else:
            raise NotImplementedError(
                f"Hint type {hint['type']} is not implemented yet."
            )

    return note_text
