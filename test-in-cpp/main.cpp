#include <tango/tango.h>

namespace MyDevice {
  class JustOneAttributeClass : public Tango::DeviceClass {
  public:
    static JustOneAttributeClass *init(const char *);
  protected:
    JustOneAttributeClass(std::string &);
    void command_factory();
    void attribute_factory(std::vector<Tango::Attr *> &);
    
    static JustOneAttributeClass *_instance;
  private:
    void device_factory(TANGO_UNUSED(const Tango::DevVarStringArray *));
    void create_static_attribute_list(std::vector<Tango::Attr *> &);
    std::vector<std::string>	defaultAttList;
  };

  class JustOneAttribute : public TANGO_BASE_CLASS
  {
  public:
    Tango::DevLong64	*attr_philipp_read;

    JustOneAttribute(Tango::DeviceClass *cl,std::string &s);
    JustOneAttribute(Tango::DeviceClass *cl,const char *s);
    JustOneAttribute(Tango::DeviceClass *cl,const char *s,const char *d);
    ~JustOneAttribute();
    
    void delete_device();
    virtual void init_device();
    virtual void always_executed_hook();

    virtual void read_attr_hardware(std::vector<long> &attr_list);
    virtual void write_attr_hardware(std::vector<long> &attr_list);

    virtual void read_philipp(Tango::Attribute &attr);
    virtual void write_philipp(Tango::WAttribute &attr);
    virtual bool is_philipp_allowed(Tango::AttReqType type);

    void add_dynamic_attributes();
    void add_dynamic_commands();
    
  };
  
  class philippAttrib: public Tango::Attr
  {
  public:
    philippAttrib():Attr("philipp",
			 Tango::DEV_LONG64, Tango::READ_WRITE) {};
    ~philippAttrib() {};
    virtual void read(Tango::DeviceImpl *dev,Tango::Attribute &att)
    {(static_cast<JustOneAttribute *>(dev))->read_philipp(att);}
    virtual void write(Tango::DeviceImpl *dev,Tango::WAttribute &att)
    {(static_cast<JustOneAttribute *>(dev))->write_philipp(att);}
    virtual bool is_allowed(Tango::DeviceImpl *dev,Tango::AttReqType ty)
    {return (static_cast<JustOneAttribute *>(dev))->is_philipp_allowed(ty);}
  };

  JustOneAttributeClass *JustOneAttributeClass::_instance = NULL;

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

  JustOneAttributeClass::JustOneAttributeClass(std::string &s):Tango::DeviceClass(s)
  {
    TANGO_LOG_INFO << "Entering JustOneAttributeClass constructor" << std::endl;
    TANGO_LOG_INFO << "Leaving JustOneAttributeClass constructor" << std::endl;
  }

  void JustOneAttributeClass::command_factory()
  {
  }

  void JustOneAttributeClass::attribute_factory(std::vector<Tango::Attr *> &att_list)
  {
    philippAttrib	*philipp = new philippAttrib();
    Tango::UserDefaultAttrProp	philipp_prop;
    //	description	not set for philipp
    philipp_prop.set_label("mylabel");
    philipp_prop.set_unit("myunit");
    philipp_prop.set_standard_unit("mystandardunit");
    philipp_prop.set_display_unit("mydisplayunit");
    //	format	not set for philipp
    //	max_value	not set for philipp
    //	min_value	not set for philipp
    //	max_alarm	not set for philipp
    //	min_alarm	not set for philipp
    //	max_warning	not set for philipp
    //	min_warning	not set for philipp
    //	delta_t	not set for philipp
    //	delta_val	not set for philipp
    philipp->set_default_properties(philipp_prop);
    //	Not Polled
    philipp->set_disp_level(Tango::OPERATOR);
    //	Not Memorized
    att_list.push_back(philipp);


    //	Create a list of static attributes
    create_static_attribute_list(get_class_attr()->get_attr_list());
    /*----- PROTECTED REGION ID(JustOneAttributeClass::attribute_factory_after) ENABLED START -----*/
    /* clang-format on */
    //	Add your own code
    /* clang-format off */
    /*----- PROTECTED REGION END -----*/	//	JustOneAttributeClass::attribute_factory_after
  }

  void JustOneAttributeClass::device_factory(const Tango::DevVarStringArray *devlist_ptr)
  {
    //	Create devices and add it into the device list
    for (unsigned long i=0 ; i<devlist_ptr->length() ; i++)
      {
	TANGO_LOG_DEBUG << "Device name : " << (*devlist_ptr)[i].in() << std::endl;
	device_list.push_back(new JustOneAttribute(this, (*devlist_ptr)[i]));
      }

    // Re-add if we have dynamic attributes
    // erase_dynamic_attributes(devlist_ptr, get_class_attr()->get_attr_list());

    //	Export devices to the outside world
    for (unsigned long i=1 ; i<=devlist_ptr->length() ; i++)
      {
	//	Add dynamic attributes if any
	JustOneAttribute *dev = static_cast<JustOneAttribute *>(device_list[device_list.size()-i]);
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
    for (unsigned long i=0 ; i<att_list.size() ; i++)
      {
	std::string att_name(att_list[i]->get_name());
	transform(att_name.begin(), att_name.end(), att_name.begin(), ::tolower);
	defaultAttList.push_back(att_name);
      }

    TANGO_LOG_INFO << defaultAttList.size() << " attributes in default list" << std::endl;

  }

  
  JustOneAttribute::JustOneAttribute(Tango::DeviceClass *cl, std::string &s)
    : TANGO_BASE_CLASS(cl, s.c_str())
  {
    init_device();
  }

  JustOneAttribute::JustOneAttribute(Tango::DeviceClass *cl, const char *s)
    : TANGO_BASE_CLASS(cl, s)
  {
    init_device();
  }
  //--------------------------------------------------------
  JustOneAttribute::JustOneAttribute(Tango::DeviceClass *cl, const char *s, const char *d)
    : TANGO_BASE_CLASS(cl, s, d)
  {
    init_device();
  }
  //--------------------------------------------------------
  JustOneAttribute::~JustOneAttribute()
  {
    delete_device();
  }

  void JustOneAttribute::delete_device()
  {
    DEBUG_STREAM << "JustOneAttribute::delete_device() " << device_name << std::endl;
    delete[] attr_philipp_read;
  }

  void JustOneAttribute::init_device()
  {
    DEBUG_STREAM << "JustOneAttribute::init_device() create device " << device_name << std::endl;
    attr_philipp_read = new Tango::DevLong64[1];
  }  

  void JustOneAttribute::always_executed_hook()
  {
  }

  void JustOneAttribute::read_attr_hardware(TANGO_UNUSED(std::vector<long> &attr_list))
  {
    DEBUG_STREAM << "JustOneAttribute::read_attr_hardware(std::vector<long> &attr_list) entering... " << std::endl;
  }

  void JustOneAttribute::write_attr_hardware(TANGO_UNUSED(std::vector<long> &attr_list))
  {
    DEBUG_STREAM << "JustOneAttribute::write_attr_hardware(std::vector<long> &attr_list) entering... " << std::endl;
  }

  void JustOneAttribute::read_philipp(Tango::Attribute &attr)
  {
    DEBUG_STREAM << "JustOneAttribute::read_philipp(Tango::Attribute &attr) entering... " << std::endl;
    this->attr_philipp_read[0] = 1337;
    attr.set_value(attr_philipp_read);
  }

//--------------------------------------------------------
/**
 *	Write attribute philipp related method
 *
 *
 *	Data type:	Tango::DevLong64
 *	Attr type:	Scalar
 */
//--------------------------------------------------------
  void JustOneAttribute::write_philipp(Tango::WAttribute &attr)
  {
    DEBUG_STREAM << "JustOneAttribute::write_philipp(Tango::WAttribute &attr) entering... " << std::endl;
    Tango::DevLong64	w_val;
    attr.get_write_value(w_val);
  }

  void JustOneAttribute::add_dynamic_attributes()
  {
  }
  
  void JustOneAttribute::add_dynamic_commands()
  {
  }
  
  bool JustOneAttribute::is_philipp_allowed(TANGO_UNUSED(Tango::AttReqType type))
  {
    return true;
  }

  
}

void Tango::DServer::class_factory()
{
  //	Add method class init if needed
  add_class(MyDevice::JustOneAttributeClass::init("JustOneAttribute"));
}

int main(int argc, char *argv[]) {
  Tango::Util *tg;
  try
    {
      // Initialise the device server
      //----------------------------------------
      tg = Tango::Util::init(argc,argv);

      // Create the device server singleton
      //	which will create everything
      //----------------------------------------
      tg->server_init(false);

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

  if(tg)
    {
      tg->server_cleanup();
    }
  return(0);
  
}
