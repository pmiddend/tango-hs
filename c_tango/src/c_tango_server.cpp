#include "c_tango.h"

namespace
{
// Copy of the Tango definition in order to use std::string for more
// memory-safety (not strictly necessary, this thing)
struct AttributeDefinitionCpp
{
  std::string name;
  TangoDataType data_type;
  AttrWriteType write_type;
  void (*set_callback)(void *);
  void (*get_callback)(void *);
};

// This is global due to our "one device per executable" convention, and stores the initial state (and can be set from a normal global C function now)
Tango::DevState initial_state;
std::string initial_status;
void (*global_finalizer_callback)(void *);

std::vector<AttributeDefinitionCpp> attribute_definitions;

struct CommandDefinitionCpp
{
  std::string command_name;
  TangoDataType in_type;
  TangoDataType out_type;
  void *(*execute_callback)(void *);
};

std::vector<CommandDefinitionCpp> command_definitions;

class HaskellCommandClass : public Tango::Command
{
public:
  HaskellCommandClass(
      std::string const &cmd_name,
      Tango::CmdArgType in,
      Tango::CmdArgType out,
      const char *in_desc,
      const char *out_desc,
      Tango::DispLevel level,
      void *(*execute_callback)(void *))
      : Command(cmd_name, in, out, in_desc, out_desc, level), execute_callback{execute_callback}
  {
  }

  CORBA::Any *execute(Tango::DeviceImpl *dev, const CORBA::Any &in_any)
  {
    if (this->get_in_type() == Tango::DEV_VOID && this->get_out_type() == Tango::DEV_VOID)
    {
      this->execute_callback(0);
      return new CORBA::Any();
    }
    else if (this->get_in_type() == Tango::DEV_LONG64 && this->get_out_type() == Tango::DEV_VOID)
    {
      Tango::DevLong64 argin;
      extract(in_any, argin);

      this->execute_callback(&argin);

      // Return value for void
      return new CORBA::Any();
    }
    else if (this->get_in_type() == Tango::DEV_STRING && this->get_out_type() == Tango::DEV_VOID)
    {
      Tango::DevString argin;
      extract(in_any, argin);

      this->execute_callback(argin);

      // Return value for void
      return new CORBA::Any();
    }
    else if (this->get_in_type() == Tango::DEV_VOID && this->get_out_type() == Tango::DEV_LONG64)
    {
      std::cout << "executing void -> long64" << std::endl;
      void *result = this->execute_callback(0);

      std::cout << "done, now casting the result" << std::endl;
      Tango::DevLong64 const number = *static_cast<Tango::DevLong64 *>(result);
      std::cout << "calling global finalizer on result\n";
      ::global_finalizer_callback(result);

      std::cout << "inserting result\n";
      return this->insert(number);
    }
    else if (this->get_in_type() == Tango::DEV_VOID && this->get_out_type() == Tango::DEV_STRING)
    {
      std::cout << "executing void -> string" << std::endl;
      char *const result = static_cast<char *>(this->execute_callback(0));
      size_t const len = strlen(result);

      std::cout << "calling global finalizer on result\n";
      char *copy = new char[len + 1];
      strncpy(copy, result, len);
      ::global_finalizer_callback(result);

      std::cout << "inserting result\n";
      // This will "delete[]" the argument, hence our copy here.
      return this->insert(copy);
    }
  }

  bool is_allowed(Tango::DeviceImpl *dev, const CORBA::Any &any) { return true; }

private:
  void *(*execute_callback)(void *);
};

class JustOneAttributeClass : public Tango::DeviceClass
{
public:
  static JustOneAttributeClass *init(const char *);
  static JustOneAttributeClass *instance();

protected:
  JustOneAttributeClass(std::string &);
  void command_factory();
  void attribute_factory(std::vector<Tango::Attr *> &);

  static JustOneAttributeClass *_instance;

private:
  void device_factory(TANGO_UNUSED(const Tango::DevVarStringArray *));
  void create_static_attribute_list(std::vector<Tango::Attr *> &);
  std::vector<std::string> defaultAttList;
};

class JustOneAttribute : public TANGO_BASE_CLASS
{
public:
  static JustOneAttribute *instance();

  JustOneAttribute(Tango::DeviceClass *cl, const char *s);
  ~JustOneAttribute();

  void delete_device();
  virtual void init_device();
  virtual void always_executed_hook();

  virtual void read_attr_hardware(std::vector<long> &attr_list);
  virtual void write_attr_hardware(std::vector<long> &attr_list);

  void add_dynamic_attributes();
  void add_dynamic_commands();

private:
  JustOneAttributeClass &parent_class;
  static JustOneAttribute *_instance;
};

JustOneAttribute *JustOneAttribute::_instance = 0;

JustOneAttribute *JustOneAttribute::instance()
{
  if (_instance == 0)
  {
    std::cerr << "Class JustOneAttribute is not initialised!" << std::endl;
    exit(-1);
  }
  return _instance;
}

class haskellAttrib : public Tango::Attr
{
public:
  haskellAttrib(AttributeDefinitionCpp const &def)
      : Attr(
            def.name.c_str(),
            static_cast<long>(def.data_type),
            Tango::OPERATOR,
            static_cast<Tango::AttrWriteType>(def.write_type)),
        def{def},
        last_ptr{0},
        last_container{0}
  {
  }

  ~haskellAttrib() {}

  virtual void read(Tango::DeviceImpl *dev, Tango::Attribute &att)
  {
    if (def.data_type == DEV_LONG64)
    {
      TangoDevLong64 int_value;
      std::cout << "haskell get callback\n";
      def.get_callback(&int_value);
      std::cout << "get callback done\n";
      std::cout << "attr get callback\n";
      att.set_value(&int_value);
      std::cout << "attr get callback done\n";
    }
    else if (def.data_type == DEV_BOOLEAN)
    {
      bool value;
      std::cout << "haskell get callback (bool)\n";
      def.get_callback(&value);
      std::cout << "get callback done\n";
      std::cout << "attr get callback (bool)\n";
      att.set_value(&value);
      std::cout << "attr get callback done\n";
    }
    else if (def.data_type == DEV_STRING)
    {
      std::cout << "haskell get callback (string)\n";
      // Future me: when you want to wrap this nicely, here's how this came to
      // be: We allocate a C string on the Haskell side, and get the pointer
      // here
      char *haskell_string;
      def.get_callback(&haskell_string);
      std::cout << "get callback done, haskell string:\n";
      std::cout << haskell_string << "\n";
      std::cout << "value output done\n";
      std::cout << "attr get callback (string)\n";

      // We also need a container for this C string, which we allocate on the
      // heap here.
      char **tango_string_array = new char *;

      // And fill it with one element
      *tango_string_array = haskell_string;

      // Then we tell Tango about this. It will remember both the container and
      // the element in it
      att.set_value(tango_string_array, 1, 0, false);

      std::cout << "attr get callback done, last ptr " << this->last_ptr << "\n";
      // If this is the second "get", the we have a container and a string
      // leftover
      if (this->last_ptr != 0)
      {
        std::cout << "freeing old ptr" << std::endl;
        // The element was allocated with Haskell's malloc (or similar) and has
        // to be deleted from Haskell as well.
        global_finalizer_callback(this->last_ptr);
        this->last_ptr = 0;
        // The other element we allocated with new, so we can delete it here
        delete static_cast<char **>(this->last_container);
        this->last_container = 0;
      }
      std::cout << "setting last ptr\n";
      this->last_ptr = haskell_string;
      this->last_container = tango_string_array;
    }
  }

  virtual void write(Tango::DeviceImpl *dev, Tango::WAttribute &att)
  {
    if (def.data_type == DEV_LONG64)
    {
      Tango::DevLong64 w_val;
      att.get_write_value(w_val);
      std::cout << "set callback " << w_val << "\n";
      def.set_callback(&w_val);
      std::cout << "set callback " << w_val << " done\n";
    }
    else if (def.data_type == DEV_BOOLEAN)
    {
      Tango::DevBoolean w_val;
      att.get_write_value(w_val);
      std::cout << "set callback " << w_val << "\n";
      def.set_callback(&w_val);
      std::cout << "set callback " << w_val << " done\n";
    }
    else if (def.data_type == DEV_STRING)
    {
      Tango::DevString w_val;
      att.get_write_value(w_val);
      std::cout << "string write value |" << w_val << "|\n";
      def.set_callback(&w_val);
      std::cout << "set callback done\n";
    }
  }

  virtual bool is_allowed(Tango::DeviceImpl *dev, Tango::AttReqType ty) { return true; }

private:
  AttributeDefinitionCpp const &def;
  void *last_ptr;
  void *last_container;
};

JustOneAttributeClass *JustOneAttributeClass::_instance = NULL;

JustOneAttributeClass *JustOneAttributeClass::instance()
{
  if (_instance == NULL)
  {
    std::cerr << "Class is not initialised !!" << std::endl;
    exit(-1);
  }
  return _instance;
}

JustOneAttributeClass *JustOneAttributeClass::init(const char *name)
{
  if (_instance == NULL)
  {
    try
    {
      std::string s(name);
      _instance = new JustOneAttributeClass(s);
    }
    catch (std::bad_alloc &)
    {
      throw;
    }
  }
  return _instance;
}

JustOneAttributeClass::JustOneAttributeClass(std::string &s) : Tango::DeviceClass(s)
{
  TANGO_LOG_INFO << "Entering JustOneAttributeClass constructor" << std::endl;
  TANGO_LOG_INFO << "Leaving JustOneAttributeClass constructor" << std::endl;
}

void JustOneAttributeClass::command_factory()
{
  std::cout << "creating commands\n";
  for (CommandDefinitionCpp const &command_definition : command_definitions)
  {
    std::cout << "adding command " << command_definition.command_name << " (in type "
              << command_definition.in_type << ")\n";
    HaskellCommandClass *tango_command = new HaskellCommandClass(
        command_definition.command_name,
        static_cast<Tango::CmdArgType>(command_definition.in_type),
        static_cast<Tango::CmdArgType>(command_definition.out_type),
        "",
        "",
        Tango::OPERATOR,
        command_definition.execute_callback);
    command_list.push_back(tango_command);
  }
  std::cout << "finish adding commands\n";
}

void JustOneAttributeClass::attribute_factory(std::vector<Tango::Attr *> &att_list)
{
  for (AttributeDefinitionCpp const &attribute_definition : attribute_definitions)
  {
    std::cout << "creating attribute " << attribute_definition.name << "\n";
    haskellAttrib *new_attribute = new haskellAttrib(attribute_definition);
    Tango::UserDefaultAttrProp attribute_prop;
    //	description	not set for philipp
    // philipp_prop.set_label("mylabel");
    // philipp_prop.set_unit("myunit");
    // philipp_prop.set_standard_unit("mystandardunit");
    // philipp_prop.set_display_unit("mydisplayunit");
    //	format	not set for philipp
    //	max_value	not set for philipp
    //	min_value	not set for philipp
    //	max_alarm	not set for philipp
    //	min_alarm	not set for philipp
    //	max_warning	not set for philipp
    //	min_warning	not set for philipp
    //	delta_t	not set for philipp
    //	delta_val	not set for philipp
    new_attribute->set_default_properties(attribute_prop);
    //	Not Polled
    new_attribute->set_disp_level(Tango::OPERATOR);
    //	Not Memorized
    att_list.push_back(new_attribute);
    std::cout << "done creating attribute " << attribute_definition.name << "\n";
  }

  //	Create a list of static attributes
  std::cout << "creating static attribute list\n";
  create_static_attribute_list(get_class_attr()->get_attr_list());
  std::cout << "creating static attribute list done\n";
}

void JustOneAttributeClass::device_factory(const Tango::DevVarStringArray *devlist_ptr)
{
  std::cout << "creating devices\n";
  //	Create devices and add it into the device list
  for (unsigned long i = 0; i < devlist_ptr->length(); i++)
  {
    TANGO_LOG_DEBUG << "Device name : " << (*devlist_ptr)[i].in() << std::endl;
    device_list.push_back(new JustOneAttribute(this, (*devlist_ptr)[i]));
  }
  std::cout << "creating devices done\n";

  // Re-add if we have dynamic attributes
  // erase_dynamic_attributes(devlist_ptr, get_class_attr()->get_attr_list());

  //	Export devices to the outside world
  for (unsigned long i = 1; i <= devlist_ptr->length(); i++)
  {
    //	Add dynamic attributes if any
    JustOneAttribute *dev = static_cast<JustOneAttribute *>(device_list[device_list.size() - i]);
    dev->add_dynamic_attributes();

    //	Check before if database used.
    if ((Tango::Util::_UseDb == true) && (Tango::Util::_FileDb == false))
      export_device(dev);
    else
      export_device(dev, dev->get_name().c_str());
  }
}

void JustOneAttributeClass::create_static_attribute_list(std::vector<Tango::Attr *> &att_list)
{
  for (unsigned long i = 0; i < att_list.size(); i++)
  {
    std::string att_name(att_list[i]->get_name());
    transform(att_name.begin(), att_name.end(), att_name.begin(), ::tolower);
    defaultAttList.push_back(att_name);
  }

  TANGO_LOG_INFO << defaultAttList.size() << " attributes in default list" << std::endl;
}

JustOneAttribute::JustOneAttribute(Tango::DeviceClass *cl, const char *s)
    : TANGO_BASE_CLASS(cl, s, "description", initial_state, initial_status),
      parent_class(static_cast<JustOneAttributeClass &>(*cl))
{
  _instance = this;
  init_device();
}

JustOneAttribute::~JustOneAttribute() { delete_device(); }

void JustOneAttribute::delete_device()
{
  DEBUG_STREAM << "JustOneAttribute::delete_device() " << device_name << std::endl;
  // delete[] attr_philipp_read;
}

void JustOneAttribute::init_device()
{
  DEBUG_STREAM << "JustOneAttribute::init_device() create device " << device_name << std::endl;
  // attr_philipp_read = new Tango::DevLong64[1];
}

void JustOneAttribute::always_executed_hook() {}

void JustOneAttribute::read_attr_hardware(TANGO_UNUSED(std::vector<long> &attr_list))
{
  DEBUG_STREAM << "JustOneAttribute::read_attr_hardware(std::vector<long> "
                  "&attr_list) entering... "
               << std::endl;
}

void JustOneAttribute::write_attr_hardware(TANGO_UNUSED(std::vector<long> &attr_list))
{
  DEBUG_STREAM << "JustOneAttribute::write_attr_hardware(std::vector<long> "
                  "&attr_list) entering... "
               << std::endl;
}

void JustOneAttribute::add_dynamic_attributes() {}

void JustOneAttribute::add_dynamic_commands() {}

// bool JustOneAttribute::is_philipp_allowed(TANGO_UNUSED(Tango::AttReqType
// type))
// {
//   return true;
// }
} // namespace

void Tango::DServer::class_factory() { add_class(JustOneAttributeClass::init("JustOneAttribute")); }

void tango_server_add_attribute_definition(AttributeDefinition *definition)
{
  attribute_definitions.push_back(AttributeDefinitionCpp{
      definition->attribute_name,
      definition->data_type,
      definition->write_type,
      definition->set_callback,
      definition->get_callback,
  });
}

void tango_server_add_command_definition(CommandDefinition *definition)
{
  std::cout << "adding command to command definitions\n";
  command_definitions.push_back(CommandDefinitionCpp{
      definition->command_name,
      definition->in_type,
      definition->out_type,
      definition->execute_callback,
  });
}

int tango_server_init(
    int argc,
    char *argv[],
    void (*global_finalizer_callback)(void *),
    char *initial_status,
    int _initial_state)
{
  // Technically bad, since we're keeping memory for this initial
  // string indefinitely (we could clear it at some point), but it's
  // much more handy than keeping the externally delivered char
  // pointer
  ::initial_status = std::string{initial_status};
  ::initial_state = static_cast<Tango::DevState>(_initial_state);
  ::global_finalizer_callback = global_finalizer_callback;

  Tango::Util *tg;
  try
  {
    // Initialise the device server
    //----------------------------------------
    tg = Tango::Util::init(argc, argv);

    // Create the device server singleton
    //	which will create everything
    //----------------------------------------
    tg->server_init(false);

    std::cout << "Server initialized" << std::endl;
  }
  catch (std::bad_alloc &)
  {
    std::cout << "Can't allocate memory to store device object !!!" << std::endl;
    std::cout << "Exiting" << std::endl;
  }
  catch (CORBA::Exception &e)
  {
    Tango::Except::print_exception(e);

    std::cout << "Received a CORBA_Exception" << std::endl;
    std::cout << "Exiting" << std::endl;
  }

  return (0);
}

void tango_server_start()
{
  Tango::Util *tg = Tango::Util::instance();
  if (!tg)
    return;
  try
  {
    // Run the endless loop
    //----------------------------------------
    std::cout << "Ready to accept request" << std::endl;
    tg->server_run();
  }
  catch (std::bad_alloc &)
  {
    std::cout << "Can't allocate memory to store device object !!!" << std::endl;
    std::cout << "Exiting" << std::endl;
  }
  catch (CORBA::Exception &e)
  {
    Tango::Except::print_exception(e);

    std::cout << "Received a CORBA_Exception" << std::endl;
    std::cout << "Exiting" << std::endl;
  }

  if (tg)
  {
    tg->server_cleanup();
  }
}

void tango_server_set_status(char *new_status)
{
  // Memeory-wise, this is fine, since set_status gets a std::string.
  JustOneAttribute::instance()->set_status(new_status);
}

void tango_server_set_state(int const new_state)
{
  JustOneAttribute::instance()->set_state(static_cast<Tango::DevState>(new_state));
}
