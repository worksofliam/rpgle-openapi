
//Used to convert noxdb schemas to structs

module.exports = class Into {
  constructor() {
      this.lines = [];
  }

  generateProcedure(object, structName) {
    this.lines.push(`Dcl-Proc ${structName};`);
    this.lines.push(`  Dcl-Pi *N LikeDS(${structName}_t);`, `    pDocument Pointer;`, `  End-Pi;`, ``);
    this.lines.push(`  Dcl-DS ${structName} LikeDS(${structName}_t);`);
    this.lines.push(`  Dcl-DS list likeds(JSON_ITERATOR);`);
    this.lines.push(`  Dcl-S lNode Pointer;`)
  
    this.generateContents(object, structName, 'pDocument');
  
    this.lines.push(``, `  Return ${structName};`, `End-Proc;`, ``);
  }

  generateContents(object, structName, variable) {
    const getTypes = {
      'number': `GetNum`,
      'string': 'GetStr',
      'boolean': 'GetInd',
      'integer': 'GetInt'
    };
  
    var currentProperty;

    switch (object.type) {
      case 'object':
        for (var name in object.properties) {
          currentProperty = object.properties[name];
      
          switch (currentProperty.type) {
            case 'number':
            case 'string':
            case 'boolean':
            case 'integer':
              this.lines.push(`  ${structName}.${name} = JSON_${getTypes[currentProperty.type]}(${variable}:'${name}');`);
              break;
      
            case 'object':
              this.lines.push('', `  lNode = JSON_Locate(${variable}:'${name}');`);
              this.generateContents(currentProperty, structName + "." + name, `lNode`);
              break;
      
            case 'array':
              this.lines.push(
                ``,
                `  ${structName}.${name}_len = 0;`,
                ``,          
                `  list = json_SetIterator(JSON_Locate(${variable}:'${name}'));`,
                `  Dow json_ForEach(list);`,
                `    ${structName}.${name}_len += 1;`,
              );
      
              if (currentProperty.items.type === "object") {
                this.generateContents(currentProperty.items, `${structName}.${name}(list.count)`, 'list.this');
              } else {
                this.lines.push(`    ${structName}.${name}(list.count) = JSON_${getTypes[currentProperty.items.type]}(list.this);`);
              }
      
              this.lines.push(`  Enddo;`);
              break;
          }
        }
        break;

      case 'array': //Only happens at a base level
        currentProperty = object;
        this.lines.push(
          ``,
          `  ${structName}.items_len = 0;`,
          ``,          
          `  list = json_SetIterator(JSON_Locate(${variable}:''));`,
          `  Dow json_ForEach(list);`,
          `    ${structName}.items_len += 1;`,
        );

        if (currentProperty.items.type === "object") {
          this.generateContents(currentProperty.items, `${structName}.items(list.count)`, 'list.this');
        } else {
          this.lines.push(`    ${structName}.items(list.count) = JSON_${getTypes[currentProperty.items.type]}(list.this);`);
        }

        this.lines.push(`  Enddo;`);
        break;
    }
  }
}
