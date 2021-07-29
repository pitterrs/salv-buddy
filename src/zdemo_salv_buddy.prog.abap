REPORT zdemo_salv_buddy.

START-OF-SELECTION.

DATA lt_data TYPE TABLE OF zma0v_setaut_pmt.
zcl_test_salv=>test(
im_data = lt_data
).
