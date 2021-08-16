# SALV Buddy

This project is implemented through the usage of the class `ZMA0_CL_SALV_BUDDY` and it intends to serve as a tool for the ones that needs CRUD operations on the standard **CL_SALV_TABLE** functionality.

It basically abstracts SALV features by encapsulating common reporting operations within a custom ABAP class that will enable its utilizer to dinamically generate a modal that will handle the creation and modification of registers being displayed.

The SALV Buddy also handles the toolbar exhibition by default and enables customization through the usage of events on specific operations at its components usage.

> **Project Details**
> 
>  - **Package:** ZMM_SALV_BUDDY
>  - **Source System:** SAP AG

## Change Management

This project wasnt transproted yet!

|Service Now Change | SAP Request |Request Description                    |
|-------------------|-------------|---------------------------------------|
|         |``       |             |

## Technical Approach

### Predefined Settings

The SALV Buddy functionality have some out-of-the-box features that are usually implemented through the usage of the standard class `CL_SALV_TABLE`:

* **Standard Toolbar Implementation**
The standard toolbar functionality it's implemented by default within the usage of the of the SALV Buddy. So there is no need for any actions to enable standard buttons on the reporting display.

* **Columns Preparation**
The columns will be automatically optimized based on the values at the output internal table and the client field (MANDT) will be set as technical to hide it on the presentation procedure.

* **CRUD Operations Available out-of-the-box**
A **create**, **update** and **delete** button will be added to the standard toolbar being displayed and its operations will be handled dinamically considering the structure being used for the output internal table.
The system will also request confirmation for any delete operation that might happen during its usage.
The creation and modification operation will make use of the function module `POPUP_GET_VALUES_USER_HELP` to dinamically build a modal screen with the fields to be created/changed during a selected operation. The same will consider key columns on the output table to manage which field will have the input option enabled or disabled at the modal presentation, *thus, it is important to maintain a strongly typed approach for the output table definition within the source program where the SALV Buddy is being used*.

### The Class Construction

To get started with the SALV Buddy functionality, it is important to understand it's components usage within the ALV implementation operation.

As you start to work with the class implementation, you will notice that the standard construction operation will be private within the class scope. This happens because the current ABAP version is unable to handle the same memory address for the output internal table using importing parameters at the method signature.

To deal with this situation, a factory method is created in order to generate an instance of the class itself. The same will receive the output data through a changing parameter at the method signature, enabling the class to handle the same memory address being used on the source program. The factory method will then use the private class constructor internally and set the output data memory address as an private attribute within the generated instance.

It is important to notice that the instance creation procedure will also need a container instance to properly display the report the way it intends to be used.

### Custom Events

It is possible to customize the behavior of some functionalities by implementing handlers using class events. These events will enable important parameters to be changed by the SALV Buddy implementer for specific requirements and scenarios.

* **ON_COLUMNS_PREPARATION**
Will be triggered through the display operation and will enable the developer to modify or enhance the columns properties considering the functionalities available at the class `CL_SALV_COLUMNS_TABLE`.

* **ON_TOOLBAR_PREPARATION**
Will be triggered through the display operation and will enable the developer to add or hide new buttons to the ALV toolbar considering the functionalities available at the class `CL_SALV_FUNCTIONS_LIST`.

## Usage Example

Here is a code with the implementation of the SALV Buddy class to serve as an example for future developments:
```
FORM display_alv.

  IF r_alv IS NOT BOUND.

    " Reserve screen space within the custom control for the ALV
    " presentation at the screen 0100
    DATA(lr_container) = NEW cl_gui_custom_container(
        container_name = 'CC_REFERENCE_DATA'
    ).

    " Create the ALV object
    r_alv = zma0_cl_salv_buddy=>factory(
      EXPORTING
          im_container = lr_container
      CHANGING
          ch_data      = t_reference_data
    ).

    " Set event for custom columns preparation
    SET HANDLER lcl_event=>handle_columns_preparation FOR r_alv.

    " Display ALV report with CRUD options
    r_alv->display( ).

  ELSE.

    r_alv->refresh( ).

  ENDIF.

ENDFORM.
```

## Caveat & More Information

> The **Eclipse 2021-03** version was used in the development phase of this project
>  You can download the latest version of the Eclipse available to work with SAP application through [this link](https://tools.hana.ondemand.com/#abap).

Considering the development procedure previously mentioned at the technical approach topic, please consider the mentioned caveats as relevant for future feature implementations and maintenance:

* Update this documentation, keep it simple, clear and always befitting reality.

The John Deere naming standard document v1.6.8 was considered during the development phase of the initial commit of this project.
To check more information related to the John Deere coding guideline you can check [this link](http://share-internal.deere.com/teams/architecturepublications/L_Supporting%20Documents/SAP%20Materials%20-%20Summary%20Page/index.htm).
