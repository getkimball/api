<script>
    import {
             Button,
             Col,
             Collapse,
             Dropdown,
             DropdownItem,
             DropdownMenu,
             DropdownToggle,
             Input,
             FormGroup,
             ListGroup,
             ListGroupItem,
             Row} from "sveltestrap";

    export let userSpec = [];

    let castTypes = [
      "string",
      "number"
    ];

    let castType;
    let isMultiValue;

    let typeCastFunMap = {};
    typeCastFunMap["string"] = String;
    typeCastFunMap["number"] = Number;

    if (typeof(userSpec.value) == "object") {
      isMultiValue = true;
      if ( userSpec.length > 0 ) {
        castType = typeof(userSpec.value[0]);
      } else {
        castType = "string";
      }
    } else {
      isMultiValue = false;
      castType = typeof(userSpec.value)
    };

    let isOpen = false;



    function castValue() {
      let typeCastFun = typeCastFunMap[castType];
      if ( isMultiValue == true ) {
        userSpec.value = userSpec.value.map(typeCastFun);
      } else  {
        userSpec.value = typeCastFun(userSpec.value);
      }
    };

    function changeComparator() {
     if (userSpec.comparator == "=" ) {
        isMultiValue = false;
        userSpec.value = userSpec.value[0];
     } else if (userSpec.comparator == "in") {
        isMultiValue = true;
        userSpec.value = [userSpec.value];
     } else {
        alert("Unknown comparator type")
     }
    };
    function addNewArrayValue() {
      let typeCastFun = typeCastFunMap[castType];
      userSpec.value = [...userSpec.value, typeCastFun("")]
    };
    function removeUserSpecArrayValue(arrayValue) {
        return function () {
            userSpec.value = userSpec.value.filter(i => i !== arrayValue);
        };
    };

</script>


<main>

  <ListGroup>
  <ListGroupItem>
      <Input bind:value={userSpec.property} />

      <FormGroup>
        <select bind:value="{userSpec.comparator}" on:change="{changeComparator}" >
          <option>=</option>
          <option>in</option>
        </select>
      </FormGroup>

      <FormGroup>
        <select bind:value={castType} on:change="{castValue}">
          {#each castTypes as type }
            <option>{type}</option>
          {/each}
        </select>
      </FormGroup>

      {#if isMultiValue }
        {#each userSpec.value as valueItem}
          <Input bind:value={valueItem} on:change="{castValue}" />
          <Button on:click="{removeUserSpecArrayValue(valueItem)}">Remove</Button>
        {/each}
        <Button on:click="{addNewArrayValue}">Add list member</Button>

      {:else}
        <Input bind:value={userSpec.value} on:change="{castValue}" />
      {/if}

  </ListGroupItem>
  </ListGroup>

</main>

<style>
</style>
