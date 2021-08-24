
//Used to create data-structures from openapi schemas

module.exports = class Structs {
  constructor() {
    this.lines = [];
  };

  generateStruct(object, structName) {
    const types = {
      'number': `Packed(30:15)`,
      'boolean': 'Ind',
      'integer': 'Int(20)'
    };
  
    var currentStruct = [];
  
    currentStruct.push(`Dcl-Ds ${structName}_t Qualified Template;`);
  
    var currentProperty;

    switch (object.type) {
      case 'object':
        for (const name in object.properties) {
          currentProperty = object.properties[name];
      
          currentStruct.push(`  //@ ${currentProperty.description || ''} ${currentProperty.nullable ? '(optional)' : ''}`);
          if (currentProperty.minLength && currentProperty.maxLength) {
            if (currentProperty.minLength === currentProperty.maxLength) {
              currentStruct.push(`  //@ Requires exactly ${currentProperty.maxLength} characters`);
            } else {
              currentStruct.push(`  //@ Length range: ${currentProperty.minLength} <=> ${currentProperty.maxLength}.`);
            }
          }
          if (currentProperty.minimum !== undefined && currentProperty.maximum) {
            currentStruct.push(`  //@ Numeric range: ${currentProperty.minimum} <=> ${currentProperty.maximum}.`);
          }
          switch (currentProperty.type) {
            case 'string':
              if (!currentProperty.default) currentProperty.default = ``;
              currentStruct.push(`  ${name} Varchar(${currentProperty.maxLength || 64}) Inz('${currentProperty.default}');`);
              break;

            case 'boolean':
              currentStruct.push(`  ${name} ${types[currentProperty.type]}${currentProperty.default !== undefined ? ` Inz(${currentProperty.default ? `*on` : `*off`})` : ``};`);
              break;

            case 'number':
            case 'integer':
              if (!currentProperty.default) currentProperty.default = `0`;
              currentStruct.push(`  ${name} ${types[currentProperty.type]}${currentProperty.default ? ` Inz(${currentProperty.default})` : ``};`);
              break;
      
            case 'object':
              this.generateStruct(currentProperty, `${structName}_${name}`);
              currentStruct.push(`  ${name} LikeDS(${structName}_${name}_t);`);
              break;
      
            case 'array':
              currentStruct.push(`  ${name}_len Uns(5) Inz(0);`);
              if (currentProperty.items.type === "object") {
                this.generateStruct(currentProperty.items, `${structName}_${name}`);
                currentStruct.push(`  ${name} LikeDS(${structName}_${name}_t) Dim(100);`);
              } else {
                currentStruct.push(`  ${name} ${types[currentProperty.items.type]} Dim(100);`);
              }
              break;

            default:
              currentStruct.push(`  //@ ${name} not supported. Maybe oneOf or anyOf.`);
              break;
          }
        }
        break;
    
      case 'array': //When the base type is an array
        currentStruct.push(`  items_len Uns(5);`);
        currentProperty = object;

        if (currentProperty.items.type === "object") {
          this.generateStruct(currentProperty.items, `${structName}_items`);
          currentStruct.push(`  items LikeDS(${structName}_items_t) Dim(100);`);
        } else {
          currentStruct.push(`  items ${types[currentProperty.items.type]} Dim(100);`);
        }
        break;
    }

    currentStruct.push(`End-Ds;`, '');
  
    this.lines.push(...currentStruct);
  }
}
