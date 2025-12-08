INTERFACE zif_zork_00_game PUBLIC.
*----------------------------------------------------------------------*
* Game Data Provider Interface
* Returns Z-machine story file data
*----------------------------------------------------------------------*

  METHODS:
    get_name
      RETURNING VALUE(rv_name) TYPE string,

    get_story
      RETURNING VALUE(rv_story) TYPE string.  " Hex-encoded story data

ENDINTERFACE.
